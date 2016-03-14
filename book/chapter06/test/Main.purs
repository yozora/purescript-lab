module Test.Main where

import Prelude ((+), (==), ($))

import Control.Monad.Eff (Eff)

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

main = runTest do
  test "First tests" do
    Assert.assert "2+2=4" $ (2 + 2) == 4
--  log "You should add some tests."
