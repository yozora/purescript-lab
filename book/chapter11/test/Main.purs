module Test.Main where

import Prelude (Unit, bind, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)

import Exercise (testParens, line, indent, cat, render
                , collatzLength, collatzSeq)

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
    suite "Indent exercise" do
      test "line" do
        Assert.equal "str" $ render (line "str")
      test "indent" do
        let doc = indent $ line "str"
        Assert.equal "  str" $ render doc
      test "cat" do
        Assert.equal "a\r\nb" $ render (cat [line "a", line "b"])
      test "integration" do
        let doc = indent $ cat
                  [ line "line1"
                  , line "line2"
                  ]
        let expected = "  line1\r\n  line2"
        Assert.equal expected $ render doc
    suite "Writer exercises" do
      test "collatzLength" do
        Assert.equal 0 $ collatzLength 1
        Assert.equal 6 $ collatzLength 10
      test "collatzSeq" do
        let expected = [10, 5, 16, 8, 4, 2, 1]
        Assert.equal expected $ collatzSeq 10
        
  log "Done."
