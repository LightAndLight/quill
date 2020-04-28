{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}
module Test.Query (queryTests) where

import Capnp.Gen.Migration.Pure
  ( AlterTable(AlterTable), Command(..)
  , Migration(Migration), Migration'parent(..)
  , TableChange(..)
  )
import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..), Result(Result))
import Capnp.Gen.Table.Pure
  ( Column(..)
  , Constraint(..)
  , Table(Table)
  )
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Void (absurd)
import GHC.Exts (fromList)
import GHC.Stack (HasCallStack)
import qualified Network.Socket as Socket
import System.Exit (exitFailure)
import Test.Hspec

import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import qualified Quill.Backend.Postgres as Postgres
import qualified Quill.Check as Check
import Quill.Normalise (Value(..))
import qualified Quill.Normalise as Normalise
import Quill.Parser (Parser)
import qualified Quill.Parser as Parser
import Quill.Syntax (Type(..))
import qualified Quill.Query as Query
import Quill.SQL (CompileError)
import qualified Quill.SQL as SQL

setupDb ::
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  (Backend -> IO ()) ->
  IO ()
setupDb setup teardown k =
  Backend.withBackend
    (\server_sock config -> do
       port <-
         (\case; Socket.SockAddrInet port _ -> pure $ show port; _ -> error "Got non-IPv4 socket address") =<<
         Socket.getSocketName server_sock
       addr <-
         head <$>
         Socket.getAddrInfo
           (Just $
           Socket.defaultHints
           { Socket.addrFamily = Socket.AF_INET
           , Socket.addrSocketType = Socket.Stream
           }
           )
           (Just "127.0.0.1")
           (Just port)
       bracket
         (Socket.socket
           (Socket.addrFamily addr)
           (Socket.addrSocketType addr)
           (Socket.addrProtocol addr)
         )
         Socket.close
         (\client_sock -> do
            Socket.connect client_sock (Socket.addrAddress addr)
            Postgres.run client_sock $
              Postgres.Config
              { Postgres._cfgPort = port
              , Postgres._cfgDbHost = Text.unpack <$> Backend._cfgDbHost config
              , Postgres._cfgDbPort = show <$> Backend._cfgDbPort config
              , Postgres._cfgDbName = Text.unpack <$> Backend._cfgDbName config
              , Postgres._cfgDbUser = Text.unpack <$> Backend._cfgDbUser config
              , Postgres._cfgDbPassword = Text.unpack <$> Backend._cfgDbPassword config
              }
         )
    )
    Backend.emptyConfig
    (\b -> bracket (pure b) teardown (\backend -> setup backend *> k backend))

exec :: Backend -> ByteString -> IO Response
exec backend = Backend.request backend . Request'exec

createTable :: Backend -> Table -> IO Response
createTable backend = Backend.request backend . Request'createTable

shouldBeDone :: HasCallStack => Response -> IO ()
shouldBeDone res =
  case res of
    Response'done -> pure ()
    _ -> error $ "Expected 'done', got: " <> show res

setupDbDecode :: (Backend -> IO ()) -> IO ()
setupDbDecode =
  setupDb
    (\backend -> do
       (shouldBeDone =<<) . createTable backend $
         Table
           "Persons"
           [ Column
             { name = "id"
             , type_ = "INT"
             , notNull = True
             , autoIncrement = True
             }
           , Column
             { name = "age"
             , type_ = "INT"
             , notNull = True
             , autoIncrement = False
             }
           , Column
             { name = "checked"
             , type_ = "BOOLEAN"
             , notNull = True
             , autoIncrement = False
             }
           ]
           [ Constraint'primaryKey ["id"] ]

       (shouldBeDone =<<) . exec backend $
         Char8.unlines
         [ "INSERT INTO Persons ( age, checked )"
         , "VALUES ( 10, true ), ( 11, false ), ( 12, true );"
         ]

       (shouldBeDone =<<) . createTable backend $
         Table
           "Nested"
           [ Column
             { name = "a"
             , type_ = "INT"
             , notNull = True
             , autoIncrement = False
             }
           , Column
             { name = "nest_b"
             , type_ = "INT"
             , notNull = True
             , autoIncrement = False
             }
           , Column
             { name = "nest_c"
             , type_ = "BOOLEAN"
             , notNull = True
             , autoIncrement = False
             }
           ]
           []

       (shouldBeDone =<<) . exec backend $
         Char8.unlines
         [ "INSERT INTO Nested ( a, nest_b, nest_c )"
         , "VALUES ( 11, 100, true ), ( 22, 200, false ), ( 33, 300, true );"
         ]

       pure ()
    )
    (\backend -> do
       shouldBeDone =<< exec backend "DROP TABLE IF EXISTS Nested;"
       shouldBeDone =<< exec backend "DROP TABLE IF EXISTS Persons;"
       shouldBeDone =<< exec backend "DROP SEQUENCE IF EXISTS Persons_id_seq;"
       pure ()
    )

setupDbBlank :: (Backend -> IO ()) -> IO ()
setupDbBlank =
  setupDb
    (\_ -> pure ())
    (\_ -> pure ())

setupDbDeleting :: [ByteString] -> (Backend -> IO ()) -> IO ()
setupDbDeleting tableNames =
  setupDb
    (\_ -> pure ())
    (\backend ->
      traverse_
        (\tableName -> shouldBeDone =<< exec backend ("DROP TABLE IF EXISTS " <> tableName <> ";"))
        tableNames
    )

setupDbQuery :: (Backend -> IO ()) -> IO ()
setupDbQuery =
  setupDb
    (\_ -> pure ())
    (\backend -> do
       shouldBeDone =<< exec backend "DROP TABLE IF EXISTS Expenses;"
       shouldBeDone =<< exec backend "DROP SEQUENCE IF EXISTS Expenses_id_seq;"
       do
         res <- exec backend "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE table_name = 'Expenses';"
         case res of
           Response'result (Result _ _ results) -> results `shouldBe` [["0"]]
           _ -> do
             putStrLn $ "Unexpected response: " <> show res
             exitFailure
       pure ()
    )

data Compile output where
  Compile ::
    (Show e, Show e', Show item, Show item', Show compileError) =>
    { compile_prelude :: [String]
    , compile_parsePrelude :: Parser prelude
    , compile_checkPrelude :: prelude -> Either e prelude'
    , compile_item :: [String]
    , compile_parseItem :: Parser item
    , compile_checkItem :: prelude' -> item -> Either e' item'
    , compile_normalise :: item' -> item'
    , compile_gen :: prelude' -> item' -> Either compileError output
    } ->
    Compile output

compile :: HasCallStack => Compile output -> IO output
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
    Right prelude' ->
      case checkPrelude prelude' of
        Left err -> error $ show err
        Right prelude'' ->
          case Parser.parseString parseItem (unlines item) of
            Left err -> error err
            Right item' ->
              case checkItem prelude'' item' of
                Left err -> error $ show err
                Right item'' -> do
                  -- print item'
                  case gen prelude'' (normalise item'') of
                    Left err -> error $ show err
                    Right code -> do
                      -- print code
                      pure code

shouldBeResult :: HasCallStack => Response -> IO Result
shouldBeResult res =
  case res of
    Response'result res' -> pure res'
    _ -> error $ "Expected result, got: " <> show res

queryTests :: Spec
queryTests = do
  around setupDbDecode . describe "decode" $ do
    it "decodeRecord 1" $ \backend -> do
      res <- exec backend "SELECT * FROM Persons;"
      let fields = [("id", TInt ()), ("age", TInt ()), ("checked", TBool ())]
      Result _ _ results <- shouldBeResult res
      one <- Query.decodeRecord (results Vector.! 0) fields
      two <- Query.decodeRecord (results Vector.! 1) fields
      three <- Query.decodeRecord (results Vector.! 2) fields
      one `shouldBe` Record [("id", Int 1), ("age", Int 10), ("checked", Bool True)]
      two `shouldBe` Record [("id", Int 2), ("age", Int 11), ("checked", Bool False)]
      three `shouldBe` Record [("id", Int 3), ("age", Int 12), ("checked", Bool True)]
    it "decodeRecord 2" $ \backend -> do
      res <- exec backend "SELECT * FROM Nested;"
      let fields = [("a", TInt ()), ("nest", TRecord () [("b", TInt ()), ("c", TBool ())])]
      Result _ _ results <- shouldBeResult res
      one <- Query.decodeRecord (results Vector.! 0) fields
      two <- Query.decodeRecord (results Vector.! 1) fields
      three <- Query.decodeRecord (results Vector.! 2) fields
      one `shouldBe` Record [("a", Int 11), ("nest", Record [("b", Int 100), ("c", Bool True)])]
      two `shouldBe` Record [("a", Int 22), ("nest", Record [("b", Int 200), ("c", Bool False)])]
      three `shouldBe` Record [("a", Int 33), ("nest", Record [("b", Int 300), ("c", Bool True)])]
  around setupDbBlank . describe "create table" $ do
    it "1" $ \backend -> do
      query <-
        compile $
        Compile
        { compile_prelude =
          [ "type AUD = { dollars : Int, cents : Int };"
          ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls Check.emptyDeclEnv . fromList
        , compile_item =
            [ "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD,"
            , "  is_food : Bool"
            , "}"
            ]
        , compile_parseItem = Parser.decl
        , compile_checkItem = \(_, env) -> Check.checkDecls env . pure
        , compile_normalise = id
        , compile_gen =
          \_ (_, env) ->
            Right @CompileError $
            SQL.compileTable
              "Expenses"
              (Maybe.fromJust . Map.lookup (Check.toLower "Expenses") $ Check._deTables env)
        }
      shouldBeDone =<< createTable backend query
      do
        res <- exec backend "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE table_name = 'expenses';"
        Result _ _ results <- shouldBeResult res
        results `shouldBe` [["1"]]
      do
        res <- exec backend "SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = 'expenses';"
        Result rs cs results <- shouldBeResult res
        rs `shouldBe` 4
        cs `shouldBe` 1
        results `shouldBe` [["id"], ["cost_dollars"], ["cost_cents"], ["is_food"]]
      _ <- exec backend "DROP TABLE Expenses;"
      do
        res <- exec backend "SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES WHERE table_name = 'expenses';"
        Result _ _ results <- shouldBeResult res
        results `shouldBe` [["0"]]
      pure ()
  around setupDbQuery . describe "insert" $ do
    it "1" $ \backend -> do
      create <-
        compile $
        Compile
        { compile_prelude =
          [ "type AUD = { dollars : Int, cents : Int };"
          ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls Check.emptyDeclEnv . fromList
        , compile_item =
            [ "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD,"
            , "  is_food : Bool"
            , "}"
            ]
        , compile_parseItem = Parser.decl
        , compile_checkItem = \(_, env) -> Check.checkDecls env . pure
        , compile_normalise = id
        , compile_gen =
          \_ (_, env) ->
            Right @CompileError $
            SQL.compileTable
              "Expenses"
              (Maybe.fromJust . Map.lookup (Check.toLower "Expenses") $ Check._deTables env)
        }
      shouldBeDone =<< createTable backend create
      insert <-
        compile $
        Compile
        { compile_prelude =
            [ "type AUD = { dollars : Int, cents : Int };"
            , "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD,"
            , "  is_food : Bool"
            , "}"
            ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls Check.emptyDeclEnv . fromList
        , compile_item =
            [ "insert { cost: { dollars: 2, cents: 3 }, is_food: True } into Expenses"
            ]
        , compile_parseItem = Parser.query (const Nothing)
        , compile_checkItem =
            \(_, env) e ->
              fmap fst $
              Check.checkExpr
                (Check.toQueryEnv env)
                e
                (TQuery Check.anyTypeInfo $ TUnit Check.anyTypeInfo)
        , compile_normalise = Normalise.normaliseExpr
        , compile_gen =
          \(_, env) e ->
            Lazy.toStrict . Builder.toLazyByteString . SQL.compileExpr <$>
            SQL.expr env absurd e
        }
      _ <- exec backend insert
      do
        Result rs cs results <- shouldBeResult =<< exec backend "SELECT * FROM Expenses;"
        rs `shouldBe` 1
        cs `shouldBe` 4
        results `shouldBe` [["1", "2", "3", "t"]]
      pure ()
  around setupDbQuery . describe "select" $ do
    it "1" $ \backend -> do
      create <-
        compile $
        Compile
        { compile_prelude =
          [ "type AUD = { dollars : Int, cents : Int };"
          ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls Check.emptyDeclEnv . fromList
        , compile_item =
            [ "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD,"
            , "  is_food : Bool"
            , "}"
            ]
        , compile_parseItem = Parser.decl
        , compile_checkItem = \(_, env) -> Check.checkDecls env . pure
        , compile_normalise = id
        , compile_gen =
          \_ (_, env) ->
            Right @CompileError $
            SQL.compileTable
              "Expenses"
              (Maybe.fromJust . Map.lookup (Check.toLower "Expenses") $ Check._deTables env)
        }
      shouldBeDone =<< createTable backend create
      select <-
        compile $
        Compile
        { compile_prelude =
            [ "type AUD = { dollars : Int, cents : Int };"
            , "table Expenses {"
            , "  id : Int, PK(id), AUTO_INCREMENT(id),"
            , "  cost : AUD,"
            , "  is_food : Bool"
            , "}"
            ]
        , compile_parsePrelude = Parser.decls
        , compile_checkPrelude =
            Check.checkDecls Check.emptyDeclEnv . fromList
        , compile_item =
            [ "select from Expenses"
            ]
        , compile_parseItem = Parser.query (const Nothing)
        , compile_checkItem =
            \(_, env) e ->
              fmap fst $
              Check.checkExpr
                (Check.toQueryEnv env)
                e
                (TQuery Check.anyTypeInfo .
                 TMany Check.anyTypeInfo $
                 TRecord Check.anyTypeInfo
                 [ ("id", TInt Check.anyTypeInfo)
                 , ( "cost"
                   , TRecord Check.anyTypeInfo
                     [ ("dollars", TInt Check.anyTypeInfo)
                     , ("cents", TInt Check.anyTypeInfo)
                     ]
                   )
                 , ("is_food", TBool Check.anyTypeInfo)
                 ]
                )
        , compile_normalise = Normalise.normaliseExpr
        , compile_gen =
          \(_, env) e ->
            Lazy.toStrict . Builder.toLazyByteString . SQL.compileExpr <$>
            SQL.expr env absurd e
        }
      shouldBeDone =<< exec backend "INSERT INTO Expenses(cost_dollars, cost_cents, is_food) VALUES (22, 33, false);"
      do
        Result rs cs results <- shouldBeResult =<< exec backend select
        rs `shouldBe` 1
        cs `shouldBe` 4
        results `shouldBe` [["1", "22", "33", "f"]]
      pure ()
  around setupDbQuery . describe "query API" $ do
    it "1" $ \backend -> do
      qe <-
        Query.loadString backend $
        unlines
        [ "type AUD = { dollars : Int, cents : Int };"
        , ""
        , "table Expenses {"
        , "  id : Int, PK(id), AUTO_INCREMENT(id),"
        , "  cost : AUD,"
        , "  is_food : Bool"
        , "}"
        , ""
        , "query insertExpense(cost: AUD, is_food: Bool) -> Query Unit {"
        , "  insert { cost: cost, is_food: is_food } into Expenses"
        , "}"
        , ""
        , "query selectExpenses() -> Query (Many { id : Int, cost : AUD, is_food : Bool }) {"
        , "  select from Expenses"
        , "}"
        ]
      Query.createTable qe "Expenses"
      insertRes <-
        Query.query
          qe
          "insertExpense"
          [Record [("dollars", Int 10), ("cents", Int 99)], Bool False]
      insertRes `shouldBe` Unit
      selectRes <-
        Query.query
          qe
          "selectExpenses"
          []
      selectRes `shouldBe`
        Many
        [ Record
          [ ("id", Int 1)
          , ("cost", Record [("dollars", Int 10), ("cents", Int 99)])
          , ("is_food", Bool False)
          ]
        ]
  around (setupDbDeleting ["quill_migrations"]) . describe "Initial migration setup works" $ do
    it "1" $ \backend -> do
      let
        m =
          Migration
            "initial"
            Migration'parent'none
            []
      shouldBeDone =<< Backend.request backend (Request'migrate m)
      Result rows cols data_ <-
        shouldBeResult =<<
        exec backend "SELECT table_name FROM information_schema.tables WHERE table_name = 'quill_migrations';"
      rows `shouldBe` 1
      cols `shouldBe` 1
      data_ `shouldBe` [["quill_migrations"]]
  around (setupDbDeleting ["quill_migrations"]) . describe "Initial migration fails the second time" $ do
    it "1" $ \backend -> do
      let
        m =
          Migration
            "initial"
            Migration'parent'none
            []
      shouldBeDone =<< Backend.request backend (Request'migrate m)
      res <- Backend.request backend (Request'migrate m)
      case res of
        Response'error val -> val `shouldBe` "ERROR:  relation \"quill_migrations\" already exists\n"
        _ -> expectationFailure $ "Expected 'error', got: " <> show res
  around (setupDbDeleting ["quill_migrations", "my_table"]) . describe "Creating a table in initial migration" $ do
    it "1" $ \backend -> do
      let
        m =
          Migration
            "initial"
            Migration'parent'none
            [ Command'createTable $
              Table
                "my_table"
                [ Column "id" "INTEGER" True True
                , Column "a" "BOOLEAN" True False
                , Column "b" "TEXT" False False
                ]
                []
            ]
      shouldBeDone =<< Backend.request backend (Request'migrate m)
      Result rows cols data_ <-
        shouldBeResult =<<
        exec backend
        ("SELECT column_name, data_type, is_nullable " <>
         "FROM information_schema.columns WHERE table_name = 'my_table';"
        )
      rows `shouldBe` 3
      cols `shouldBe` 3
      data_ `shouldBe`
        [ ["id", "integer", "NO"]
        , ["a", "boolean", "NO"]
        , ["b", "text", "YES"]
        ]
  around (setupDbDeleting ["quill_migrations", "my_table"]) . describe "Add autoincrementable field in migration" $ do
    it "1" $ \backend -> do
      let
        m1 =
          Migration
            "initial"
            Migration'parent'none
            [ Command'createTable $
              Table
                "my_table"
                [ Column "id" "INTEGER" True True
                , Column "a" "BOOLEAN" True False
                , Column "b" "TEXT" False False
                ]
                []
            ]
        m2 =
          Migration
            "first"
            (Migration'parent'some "initial")
            [ Command'alterTable $
              AlterTable
                "my_table"
                [ TableChange'addColumn $ Column "c" "INTEGER" True True
                ]
            ]
      shouldBeDone =<< Backend.request backend (Request'migrate m1)
      shouldBeDone =<< Backend.request backend (Request'migrate m2)
      Result rows cols data_ <-
        shouldBeResult =<<
        exec backend
        ("SELECT column_name, data_type, is_nullable " <>
         "FROM information_schema.columns WHERE table_name = 'my_table';"
        )
      rows `shouldBe` 4
      cols `shouldBe` 3
      data_ `shouldBe`
        [ ["id", "integer", "NO"]
        , ["a", "boolean", "NO"]
        , ["b", "text", "YES"]
        , ["c", "integer", "NO"]
        ]
