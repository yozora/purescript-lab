module Main where

import Prelude (Unit, class Applicative, pure, (<*>), (<$>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(..)) 

combineList :: forall f a. (Applicative f) => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> (combineList xs)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
