{-# language OverloadedLists, OverloadedStrings #-}
module Test.Query (queryTests) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as Char8
import Database.PostgreSQL.LibPQ as Postgres
import System.Exit (exitFailure)
import Test.Hspec

import Quill.Normalise (Value(..))
import Quill.Syntax (Type(..))
import qualified Quill.Query as Query

notError :: IO (Maybe ByteString) -> IO ()
notError m =
  m >>= \m_res ->
  case m_res of
    Just "" -> pure ()
    Just message -> do
      Char8.putStr message
      undefined
    Nothing -> pure ()

setupDb :: (Postgres.Connection -> IO ()) -> IO ()
setupDb k =
  bracket
    (Postgres.connectdb "")
    (\conn -> do
        Postgres.exec conn "DROP TABLE Nested;"
        Postgres.exec conn "DROP TABLE Persons;"
        Postgres.exec conn "DROP SEQUENCE Persons_id_seq;"
        notError $ Postgres.errorMessage conn
        Postgres.finish conn
    )
    (\conn -> do
       notError $ Postgres.errorMessage conn

       Postgres.exec conn "CREATE SEQUENCE Persons_id_seq;"
       notError $ Postgres.errorMessage conn

       Postgres.exec conn $
         Char8.unlines
         [ "CREATE TABLE Persons ("
         , "id INT PRIMARY KEY DEFAULT nextval('Persons_id_seq'),"
         , "age INT NOT NULL,"
         , "checked BOOLEAN NOT NULL"
         , ");"
         ]
       notError $ Postgres.errorMessage conn

       Postgres.exec conn $
         Char8.unlines
         [ "INSERT INTO Persons ( age, checked )"
         , "VALUES ( 10, true ), ( 11, false ), ( 12, true );"
         ]
       notError $ Postgres.errorMessage conn

       Postgres.exec conn $
         Char8.unlines
         [ "CREATE TABLE Nested ("
         , "a INT NOT NULL,"
         , "nest_b INT NOT NULL,"
         , "nest_c BOOLEAN NOT NULL"
         , ");"
         ]
       notError $ Postgres.errorMessage conn

       Postgres.exec conn $
         Char8.unlines
         [ "INSERT INTO Nested ( a, nest_b, nest_c )"
         , "VALUES ( 11, 100, true ), ( 22, 200, false ), ( 33, 300, true );"
         ]
       notError $ Postgres.errorMessage conn

       k conn
    )

queryTests :: Spec
queryTests =
  around setupDb $
  describe "query" $ do
    it "decodeRecord 1" $ \conn -> do
      res <- maybe undefined pure =<< Postgres.exec conn "SELECT * FROM Persons;"
      let fields = [("id", TInt ()), ("age", TInt ()), ("checked", TBool ())]
      one <- Query.decodeRecord res 0 fields
      two <- Query.decodeRecord res 1 fields
      three <- Query.decodeRecord res 2 fields
      one `shouldBe` Record [("id", Int 1), ("age", Int 10), ("checked", Bool True)]
      two `shouldBe` Record [("id", Int 2), ("age", Int 11), ("checked", Bool False)]
      three `shouldBe` Record [("id", Int 3), ("age", Int 12), ("checked", Bool True)]
    it "decodeRecord 2" $ \conn -> do
      res <- maybe undefined pure =<< Postgres.exec conn "SELECT * FROM Nested;"
      let fields = [("a", TInt ()), ("nest", TRecord () [("b", TInt ()), ("c", TBool ())])]
      one <- Query.decodeRecord res 0 fields
      two <- Query.decodeRecord res 1 fields
      three <- Query.decodeRecord res 2 fields
      one `shouldBe` Record [("a", Int 11), ("nest", Record [("b", Int 100), ("c", Bool True)])]
      two `shouldBe` Record [("a", Int 22), ("nest", Record [("b", Int 200), ("c", Bool False)])]
      three `shouldBe` Record [("a", Int 33), ("nest", Record [("b", Int 300), ("c", Bool True)])]
