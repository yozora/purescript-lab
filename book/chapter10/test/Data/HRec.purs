module Test.Data.HRec where

import Prelude (Unit, (++), bind)
import Data.Maybe (Maybe(Just))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert

import Data.HRec (empty, insert, foldHRec, union, lookup)

import Data.Function ( runFn3, mkFn3 )


main :: forall a. Control.Monad.Free.Free (Test.Unit.TestF a) Unit 
main = do
  suite "Test.Data.HRec" do
    test "Lookup" do
      Assert.equal (lookup "a" (runFn3 insert "a" 1 empty)) (Just 1)
    test "Union" do
      let r1 = (runFn3 insert "a" 1 (runFn3 insert "b" 1 empty))
      let r2 = (runFn3 insert "c" 2 (runFn3 insert "b" 2 empty))
      let r = union r1 r2
      Assert.equal (lookup "a" r) (Just 1)
      Assert.equal (lookup "b" r) (Just 2)
      Assert.equal (lookup "c" r) (Just 2)
    test "Empty" do
      Assert.equal (foldHRec combine "" empty) "" 
      where
        combine = \r k v -> r ++ k ++ v

