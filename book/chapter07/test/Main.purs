module Test.Main where

import Data.Maybe (Maybe(..))
import Prelude 

import Exercises (combineMaybe)

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

main = runTest do
  test "Basic" do
    Assert.equal (2 + 2) 4
    Assert.assert "Basic" $ 2 == 2
  test "Exercises" do
    Assert.assert "combineMaybe with list" $
      (combineMaybe (Just [1, 2])) == [(Just 1), (Just 2)]
    Assert.assert "combineMaybe with Nothing" $
      (combineMaybe (Nothing :: Maybe (Array Int))) == [Nothing]
