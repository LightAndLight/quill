{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language TypeApplications #-}
module Main where

import Test.Hspec

import Test.Compile (compileTests)
import Test.Check (convertTests, checkTests, doesn'tCheckTests)
import Test.Parse (parseTests)

main :: IO ()
main =
  hspec $ do
    describe "compile" compileTests
    describe "parse" parseTests
    describe "convert" convertTests
    describe "check" checkTests
    describe "doesn't check" doesn'tCheckTests
