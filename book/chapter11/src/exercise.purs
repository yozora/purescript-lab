module Exercise
       ( testParens
       , Level, Doc, line, indent, cat, render
       , collatzLength, collatzSeq
       , safeDivide
       , runParser, string, abParser, absParser
       ) where

import Prelude (Unit, bind, return, flip, (<<<), map, show
               , ($), (+), (-), (*), (/), (++), eq, (==), mod)
import Data.Array (replicate, snoc)
import Data.Maybe (Maybe(..))
import Data.String (split, joinWith, stripPrefix, drop, take)
import Data.Foldable (traverse_, fold)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(), fst, snd)
import Data.Identity (Identity(..), runIdentity)
import Data.Either (Either(..))
import Data.List (some)
import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Control.Monad.State (State, execState, modify, get, put)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.State.Trans (StateT(), runStateT)
import Control.Monad.Writer.Trans (WriterT(), runWriterT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError)

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
    collatz y | (y `mod` 2 == 0) =
      y /2
    collatz y =
      y * 3 + 1

collatzLength :: Int -> Int
collatzLength = fst <<< runWriter <<< collatzLengthImpl

collatzSeq :: Int -> Array Int
collatzSeq = snd <<< runWriter <<< collatzLengthImpl


safeDivide :: Number -> Number -> ExceptT String Identity Number
safeDivide n 0.0 = ExceptT <<< Identity $ Left "Divide by 0"
safeDivide n d = ExceptT <<< Identity $ Right (n / d)


type Errors = Array String

type Log = Array String

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

runParser :: Parser String -> String -> Either Errors (Tuple (Tuple String String) Log)
runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

next :: Parser String
next = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError ["Empty String"]
    _ -> do
      put (drop 1 s)
      return (take 1 s)

satisfy :: (String -> Boolean) -> Parser String
satisfy pred = do
  s <- next
  guard $ pred s
  return s
      
string :: String -> Parser String
string pre = do
  s <- get
  tell [ "The state is " ++ show s ]
  case (stripPrefix pre s) of
    (Just suf) -> do
      put (suf)
      return (pre)
    _ -> do
      throwError ["No match"]

abParser :: Parser String
abParser = do
  as <- some $ satisfy (eq "a")
  bs <- some $ satisfy (eq "b")
  return $ fold (as ++ bs)

absParser :: Parser String
absParser = 
  map fold $ some $ aParser <|> bParser
  where
    aParser = map fold $ some $ satisfy (eq "a")
    bParser = map fold $ some $ satisfy (eq "b")

