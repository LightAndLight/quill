{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language TypeApplications #-}
module Main where

import Test.Hspec

import Test.Compile (compileTests)
import Test.Check (convertTests, checkTests, doesn'tCheckTests)
import Test.Graph (graphTests)
import qualified Test.Check.Migration as Migration (checkTests)
import Test.Migrate (migrateTests)
import Test.Parse (parseTests)
import Test.Query (queryTests)

main :: IO ()
main =
  hspec $ do
    describe "compile" compileTests
    describe "graph" graphTests
    describe "parse" parseTests
    describe "convert" convertTests
    describe "check" checkTests
    describe "doesn't check" doesn'tCheckTests
    describe "migration" $ do
      describe "check" Migration.checkTests
      migrateTests
    queryTests
