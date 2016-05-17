module Control.Monad.Eff.Storage
       ( STORAGE
       , getItem, setItem
       ) where

import Prelude (Unit())

import Data.Foreign (Foreign)
import Control.Monad.Eff (Eff)

foreign import data STORAGE :: !

foreign import getItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Foreign

foreign import setItem :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit

foreign import removeItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Unit

