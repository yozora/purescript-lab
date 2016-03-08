module Main where

import Prelude

fact :: Int -> Int
fact n =
  go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (acc * n)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

pairs :: Int -> Array (Array Int)
pairs n = do
  i <- Data.Array.range 1 n
  j <- Data.Array.range 1 n
  return [i, j]
