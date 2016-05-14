module Test.Main where

import Prelude (Unit, bind, ($))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result(), (===))
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(), runAlphaNumString)

import Data.URI as URI

main :: forall eff. Eff
          ( console :: CONSOLE
          , testOutput :: Test.Unit.Console.TESTOUTPUT
          , random :: Control.Monad.Eff.Random.RANDOM
          | eff
          ) Unit
main = runTest do
  uriTests
  Test.Data.HRec.main

uriTests :: forall a. Control.Monad.Free.Free
              ( Test.Unit.TestF
                ( random :: Control.Monad.Eff.Random.RANDOM
                | a
                )
              ) Unit
uriTests = do
  suite "Uri Tests" do
    test "encodeUriComponent" do
      Assert.equal (URI.encodeUriComponent "Hello World") "Hello%20World"

    test "encodeUriComponent >>> decodeUriComponent === id" do
      quickCheck encodeUriReversable
      where
        encodeUriReversable :: AlphaNumString -> Result
        encodeUriReversable ans =
          let str = runAlphaNumString ans in
          str === URI.decodeUriComponent $ URI.encodeUriComponent str
