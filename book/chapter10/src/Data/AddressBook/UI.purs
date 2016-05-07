module Data.AddressBook.UI where

import Prelude

import Data.AddressBook (person, address, phoneNumber, PhoneType(..)
                        , Person)
import Data.AddressBook.Validation (validatePerson')

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import DOM (DOM)
import Data.Foreign.Class (read)
import Data.Traversable (sequence)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.DOM (querySelector, getValue, createElement, addClass
                             , appendChild, setText, setInnerHTML, body
                             , addEventListener)
import Control.Monad.Eff.Console (CONSOLE, log, print)

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

displayValidationErrors :: forall eff. Array String -> Eff (dom :: DOM | eff) Unit
displayValidationErrors errs = do
   alert <- createElement "div"
     >>= addClass "alert"
     >>= addClass "alert-danger"

   ul <- createElement "ul"
   appendChild ul alert

   foreachE errs $ \err -> do
     li <- createElement "li" >>= setText err
     appendChild li ul
     return unit

   Just validationErrors <- querySelector "#validationErrors"
   appendChild alert validationErrors

   return unit

validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff)
                                    (Either (Array String) Person)
validateControls = do
  log "Running validators"

  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           ]
  return $ validatePerson' p

validateAndUpdateUI :: forall eff. Eff(console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrors errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  body >>= addEventListener "change" validateAndUpdateUI
  return unit
