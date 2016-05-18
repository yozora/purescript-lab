module Test.Main where

import Prelude (Unit, bind, (<>), ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)

import Exercise (testParens)

main :: forall e. Eff (console :: CONSOLE,
                       testOutput :: TESTOUTPUT | e) Unit
main = do
  Test.Unit.Main.runTest do
    suite "testParens" do
      let t1 = "()"
      test t1 do
        Assert.equal true (testParens t1)
      let t2 = "(()(())())"
      test t2 do
        Assert.equal true $ testParens t2
      let t3 = ")"
      test t3 do
        Assert.equal false $ testParens t3
      let t4 = "(()()"
      test t4 do
        Assert.equal false $ testParens t4
        
  log "Done."
