module Exercise (testParens)
       where

import Prelude (Unit, ($), (+), (-), eq)
import Data.Maybe (Maybe(Just))
import Data.String (split)
import Data.Array (head, tail)
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (traverse_)
import Control.Monad.State (State, execState, modify)

testParens :: String -> Boolean
testParens str =
  eq 0 $ execState (traverse_ count (split "" str)) 0
  where
    count :: String -> State Int Unit
    count "(" = modify $ \x -> x + 1
    count ")" = modify $ \x -> x - 1
    count _ = modify $ \(x::Int) -> x
