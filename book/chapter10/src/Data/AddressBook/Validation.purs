module Data.AddressBook.Validation where

import Prelude (Unit, unit, pure, show, flip, ($), (<*>), (<$>), (/=))

import Data.AddressBook (Address(..), PhoneNumber(..), Person(..)
                        , address, phoneNumber, person)

import Data.Either (Either(..))
import Data.Validation (V(), invalid, runV)
import Control.Apply ((*>))
import Data.Traversable (traverse)

import Data.String as S
import Data.String.Regex as Regex

type Errors = Array String

-- Error messages

emptyMessage :: String -> String
emptyMessage = (flip (S.replace "%1")) "Field '%1' cannot be empty." 

nonEmptyMessage :: String -> String
nonEmptyMessage = (flip (S.replace "%1")) template
                  where template = "Field '%1' must contain at least one value."

lengthMessage :: Int -> String -> String
lengthMessage len = (flip (S.replace "%1")) (S.replace "%2" (show len) template)
                    where template = "Field '%1' must have length %2."

matchesMessage :: String -> String
matchesMessage = (flip $ S.replace "%1") template
                  where template = "Field '%1' did not match the required format."

-- Validators

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid [emptyMessage field]
nonEmpty _ _ = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [nonEmptyMessage field]
arrayNonEmpty _ _ = pure unit

lengthIs :: Int -> String -> String -> V Errors Unit
lengthIs len field value | S.length value /= len =
  invalid [lengthMessage len field]
lengthIs _ _ _ = pure unit

phoneNumberRegex :: Regex.Regex
phoneNumberRegex = Regex.regex "^\\d{3}-\\d{3}-\\d{4}$" Regex.noFlags

matches :: Regex.Regex -> String -> String -> V Errors Unit
matches regex _ value | Regex.test regex value = pure unit
matches _ field _ = invalid [matchesMessage field]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "street" o.street *> pure o.street)
          <*> (nonEmpty "city"   o.city   *> pure o.street)
          <*> (lengthIs 2 "state" o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches phoneNumberRegex "number" o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "firstname" o.firstname *> pure o.firstname)
         <*> (nonEmpty "lastname" o.lastname *> pure o.lastname)
         <*> validateAddress o.address
         <*> (arrayNonEmpty "phones" o.phones *>
              traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = runV Left Right $ validatePerson p
