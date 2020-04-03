{-# language OverloadedStrings #-}
module Test.Query (queryTests) where

import Control.Exception (bracket)
import Data.ByteString.Char8 as Char8
import Database.PostgreSQL.LibPQ as Postgres
import Test.Hspec

setupDb :: (Postgres.Connection -> IO ()) -> IO ()
setupDb =
  bracket
    (do
       conn <- Postgres.connectdb "host=? database=? user=? password=?"
       pure conn
    )
    Postgres.finish

queryTests :: Spec
queryTests =
  around setupDb $
  describe "query" $ do
    it "select 1" $ \conn ->
      () `shouldBe` ()
