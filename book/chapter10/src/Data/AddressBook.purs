module Data.AddressBook
  ( Address(..)
  , address
  , PhoneType(..)
  , PhoneNumber(..)
  , phoneNumber
  , Person(..)
  , person
  , examplePerson
  ) where

import Prelude (class Show, show, (++))

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String }

address :: String -> String -> String -> Address
address street city state = Address
  { street: street
  , city: city
  , state: state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber = PhoneNumber
  { "type" :: PhoneType
  , number :: String }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber phoneType number = PhoneNumber
  { "type": phoneType
  , number: number }

newtype Person = Person
  { firstname :: String
  , lastname :: String
  , address :: Address
  , phones :: Array PhoneNumber }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstname lastname addr phones = Person
  { firstname: firstname
  , lastname: lastname
  , address: addr
  , phones: phones }

examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
         [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000" ]

instance showAddress :: Show Address where
  show (Address o) = "Address " ++
    "{ street: " ++ (show o.street) ++
    ", city: " ++ (show o.city) ++
    ", state: " ++ (show o.state) ++ " }"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber o) = "PhoneNumber " ++
    "{ type: " ++ (show o."type") ++
    ", number: " ++ (show o.number) ++ " }"

instance showPerson :: Show Person where
  show (Person o) = "Person " ++ 
    "{ firstname: " ++ (show o.firstname) ++
    ", lastname: " ++ (show o.lastname) ++
    ", address: " ++ (show o.address) ++
    ", phones: " ++ (show o.phones) ++ " }"
