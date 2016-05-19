module Exercise
       ( testParens
       , Level, Doc, line, indent, cat, render
       , collatzLength, collatzSeq
       ) where

import Prelude (Unit, bind, return, flip, (<<<)
               , ($), (+), (-), (*), (/), eq, (==), mod)
import Data.Array (replicate, snoc)
import Data.String (split, joinWith)
import Data.Foldable (traverse_)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (Writer, tell, runWriter)

testParens :: String -> Boolean
testParens str =
  eq 0 $ execState (traverse_ count (split "" str)) 0
  where
    count :: String -> State Int Unit
    count "(" = modify $ \x -> x + 1
    count ")" = modify $ \x -> x - 1
    count _ = modify $ \(x::Int) -> x

type Level = Int

type Doc = Reader Level String

line :: String -> Doc
line str = do
  level <- ask
  return $ joinWith "" (snoc (replicate level "  ") str)

indent :: Doc -> Doc
indent = local (\x -> x + 1)

cat :: Array Doc -> Doc
cat docs = do
  strs <- sequence docs
  return (joinWith "\r\n" strs)

render :: Doc -> String
render = flip runReader 0

collatzLengthImpl :: Int -> Writer (Array Int) Int
collatzLengthImpl = helper 0
  where
    helper :: Int -> Int -> Writer (Array Int) Int
    helper acc 1 = do
      tell [ 1 ]
      return acc
    helper acc n = do
      tell [ n ]
      helper (acc + 1) (collatz n)

    collatz :: Int -> Int 
    collatz n =
      case n of
        y | y `mod` 2 == 0 -> y / 2
        y -> y * 3 + 1

collatzLength :: Int -> Int
collatzLength = fst <<< runWriter <<< collatzLengthImpl

collatzSeq :: Int -> Array Int
collatzSeq = snd <<< runWriter <<< collatzLengthImpl
