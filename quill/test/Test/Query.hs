{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language TypeApplications #-}
module Test.Query (queryTests) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.Exts (fromList)
import System.Exit (exitFailure)
import Test.Hspec

import qualified Quill.Check as Check
import Quill.Normalise (Value(..))
import Quill.Parser (Parser)
import qualified Quill.Parser as Parser
import Quill.Syntax (Type(..))
import qualified Quill.Syntax as Syntax
import qualified Quill.Query as Query
import Quill.SQL (CompileError)
import qualified Quill.SQL as SQL

notError :: IO (Maybe ByteString) -> IO ()
notError m =
  m >>= \m_res ->
  case m_res of
    Just "" -> pure ()
    Just message -> do
      Char8.putStr message
      undefined
    Nothing -> pure ()

setupDb ::
  (Postgres.Connection -> IO ()) ->
  (Postgres.Connection -> IO ()) ->
  (Postgres.Connection -> IO ()) ->
  IO ()
setupDb setup teardown k =
  bracket
    (Postgres.connectdb "")
    (\conn -> do
        teardown conn
        notError $ Postgres.errorMessage conn
        Postgres.finish conn
    )
    (\conn -> do
       setup conn
       k conn
    )

setupDbDecode :: (Postgres.Connection -> IO ()) -> IO ()
setupDbDecode =
  setupDb
    (\conn -> do
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
    )
    (\conn -> do
       Postgres.exec conn "DROP TABLE Nested;"
       Postgres.exec conn "DROP TABLE Persons;"
       Postgres.exec conn "DROP SEQUENCE Persons_id_seq;"
       pure ()
    )

setupDbCreateTable :: (Postgres.Connection -> IO ()) -> IO ()
setupDbCreateTable =
  setupDb
    (\_ -> pure ())
    (\_ -> pure ())

data Compile where
  Compile ::
    (Show e, Show e', Show item', Show compileError) =>
    { compile_prelude :: [String]
    , compile_parsePrelude :: Parser prelude
    , compile_checkPrelude :: prelude -> Either e prelude'
    , compile_item :: [String]
    , compile_parseItem :: Parser item
    , compile_checkItem :: prelude' -> item -> Either e' item'
    , compile_normalise :: item' -> item'
    , compile_gen :: prelude' -> item' -> Either compileError ByteString
    } ->
    Compile

compile :: Compile -> IO ByteString
compile
  (Compile
   { compile_prelude = prelude
   , compile_parsePrelude = parsePrelude
   , compile_checkPrelude = checkPrelude
   , compile_item = item
   , compile_parseItem = parseItem
   , compile_checkItem = checkItem
   , compile_gen = gen
   , compile_normalise = normalise
   }
  ) =
  case Parser.parseString parsePrelude (unlines prelude) of
    Left err -> error err
    Right prelude ->
      case checkPrelude prelude of
        Left err -> error $ show err
        Right prelude' ->
          case Parser.parseString parseItem (unlines item) of
            Left err -> error err
            Right item ->
              case checkItem prelude' item of
                Left err -> error $ show err
                Right item' -> do
                  case gen prelude' (normalise item') of
                    Left err -> error $ show err
                    Right code -> pure code

queryTests :: Spec
queryTests = do
  around setupDbDecode . describe "decode" $ do
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
  around setupDbCreateTable . describe "create table" $ do
    it "1" $ \conn -> do
      query <-
        compile $
        Compile
        { compile_prelude =
          [ "type AUD = { dollars : Int, cents : Int };"
          ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls (Check.emptyDeclEnv Syntax.SQL2003) . fromList
        , compile_item =
            [ "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD"
            , "}"
            ]
        , compile_parseItem = Parser.decls
        , compile_checkItem = \(_, env) -> Check.checkDecls env . fromList
        , compile_normalise = id
        , compile_gen =
          \_ (ds, env) ->
            Right @CompileError . Lazy.toStrict .
            Builder.toLazyByteString $
            SQL.compileDecls env ds
        }
      m_res <- Postgres.exec conn query
      case m_res of
        Nothing ->
          maybe
            (expectationFailure "Unknown database failure")
            ((*> expectationFailure "Database error") . Char8.putStr) =<<
            Postgres.errorMessage conn
        Just res -> do
          m_err <- Postgres.resultErrorMessage res
          case m_err of
            Just err -> do
              Char8.putStr err
              expectationFailure "Database error"
            Nothing -> pure ()
