module Main where

import Prelude (Unit, bind)

import Data.AddressBook.UI (setupEventHandlers)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Attaching event handlers"
  setupEventHandlers
