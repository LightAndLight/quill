{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language TypeApplications #-}
module Test.Compile (compileTests) where

import Capnp.Gen.Table.Pure
  ( Column(..)
  , Table(Table)
  )
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Void (absurd)
import GHC.Exts (fromList)
import Test.Hspec

import Quill.Check (DeclEnv(..), QueryEnv(..), anyTypeInfo)
import qualified Quill.Check as Check
import Quill.Normalise (normaliseExpr)
import Quill.Parser (Parser)
import qualified Quill.Parser as Parser
import Quill.Syntax (Type(..))
import qualified Quill.SQL as SQL

data CompileTest where
  CompileTest ::
    (Show e, Show e', Show item', Show compileError, Eq output, Show output) =>
    { compile_prelude :: [String]
    , compile_parsePrelude :: Parser prelude
    , compile_checkPrelude :: prelude -> Either e prelude'
    , compile_item :: [String]
    , compile_parseItem :: Parser item
    , compile_checkItem :: prelude' -> item -> Either e' item'
    , compile_normalise :: item' -> item'
    , compile_gen :: prelude' -> item' -> Either compileError output
    , compile_output :: output
    } ->
    CompileTest

compileTest :: HasCallStack => CompileTest -> IO ()
compileTest
  (CompileTest
   { compile_prelude = prelude
   , compile_parsePrelude = parsePrelude
   , compile_checkPrelude = checkPrelude
   , compile_item = item
   , compile_parseItem = parseItem
   , compile_checkItem = checkItem
   , compile_gen = gen
   , compile_normalise = normalise
   , compile_output = output
   }
  ) =
  case Parser.parseString parsePrelude (unlines prelude) of
    Left err -> expectationFailure err
    Right prelude' ->
      case checkPrelude prelude' of
        Left err -> expectationFailure $ show err
        Right prelude'' ->
          case Parser.parseString parseItem (unlines item) of
            Left err -> expectationFailure err
            Right item' ->
              case checkItem prelude'' item' of
                Left err -> expectationFailure $ show err
                Right item'' -> do
                  -- print (normalise item')
                  case gen prelude'' (normalise item'') of
                    Left err -> expectationFailure $ show err
                    Right code -> code `shouldBe` output

compileTests :: Spec
compileTests = do
  it "1" $
    compileTest $
    CompileTest
    { compile_prelude =
      [ "type AUD = { dollars : Int, cents : Int };"
      , "table Expenses {"
      , "  id : Int,"
      , "  cost : AUD"
      , "}"
      ]
    , compile_parsePrelude = Parser.decls
    , compile_checkPrelude =
        Check.checkDecls Check.emptyDeclEnv . fromList
    , compile_item =
        [ "expenses <- select from Expenses;"
        , "return ("
        , "  for expense in expenses"
        , "  yield expense.cost.dollars"
        , ")"
        ]
    , compile_parseItem = Parser.query (const Nothing)
    , compile_checkItem =
      \(_, env) e ->
        let
          queryEnv =
            QueryEnv
            { _qeLanguage = _deLanguage env
            , _qeNames = absurd
            , _qeLocals = absurd
            , _qeGlobalVars = _deGlobalVars env
            , _qeGlobalQueries = _deGlobalQueries env
            , _qeTypes = _deTypes env
            , _qeTables = _deTables env
            }
        in
          fmap fst . Check.checkExpr queryEnv e $
          TQuery anyTypeInfo (TMany anyTypeInfo $ TInt anyTypeInfo)
    , compile_normalise = normaliseExpr
    , compile_gen =
      \(_, env) e ->
        Lazy.toStrict . Builder.toLazyByteString . SQL.compileQuery <$>
        SQL.query env absurd e
    , compile_output = "SELECT expense.cost_dollars FROM (SELECT * FROM Expenses) AS expense"
    }
  it "2" $
    compileTest $
    CompileTest
    { compile_prelude =
      [ "type AUD = { dollars : Int, cents : Int };"
      , "table Expenses {"
      , "  id : Int,"
      , "  cost : AUD"
      , "}"
      ]
    , compile_parsePrelude = Parser.decls
    , compile_checkPrelude =
        Check.checkDecls Check.emptyDeclEnv . fromList
    , compile_item =
        [ "expenses <- select from Expenses;"
        , "return ("
        , "  for expense in expenses"
        , "  yield expense.cost"
        , ")"
        ]
    , compile_parseItem = Parser.query (const Nothing)
    , compile_checkItem =
      \(_, env) e ->
        let
          queryEnv =
            QueryEnv
            { _qeLanguage = _deLanguage env
            , _qeNames = absurd
            , _qeLocals = absurd
            , _qeGlobalVars = _deGlobalVars env
            , _qeGlobalQueries = _deGlobalQueries env
            , _qeTypes = _deTypes env
            , _qeTables = _deTables env
            }
        in
          fmap fst . Check.checkExpr queryEnv e $
          TQuery anyTypeInfo (TMany anyTypeInfo $ TName anyTypeInfo "AUD")
    , compile_normalise = normaliseExpr
    , compile_gen =
      \(_, env) e ->
        Lazy.toStrict . Builder.toLazyByteString . SQL.compileQuery <$>
        SQL.query env absurd e
    , compile_output = "SELECT expense.cost_dollars, expense.cost_cents FROM (SELECT * FROM Expenses) AS expense"
    }
  it "3" $
    compileTest $
    CompileTest
    { compile_prelude =
        [ "type AUD = { dollars : Int, cents : Int };" ]
    , compile_parsePrelude = Parser.decls
    , compile_checkPrelude =
        Check.checkDecls Check.emptyDeclEnv . fromList
    , compile_item =
        [ "table Expenses {"
        , "  id : Int,"
        , "  cost : AUD"
        , "}"
        ]
    , compile_parseItem = Parser.decl
    , compile_checkItem =
      \(_, env) -> Check.checkDecls env . pure
    , compile_normalise = id
    , compile_gen =
        \_ (_, env) ->
          Right @SQL.CompileError $
          SQL.compileTable
            (Check.toLower "Expenses")
            (Maybe.fromJust . Map.lookup (Check.toLower "Expenses") $ Check._deTables env)
    , compile_output =
      Table
        "expenses"
        [ Column
          { name = "id"
          , type_ = "INTEGER"
          , notNull = True
          , autoIncrement = False
          }
        , Column
          { name = "cost_dollars"
          , type_ = "INTEGER"
          , notNull = True
          , autoIncrement = False
          }
        , Column
          { name = "cost_cents"
          , type_ = "INTEGER"
          , notNull = True
          , autoIncrement = False
          }
        ]
        []
    }
  it "4" $
    compileTest $
    CompileTest
    { compile_prelude =
        [ "type AUD = { dollars : Int, cents : Int };" ]
    , compile_parsePrelude = Parser.decls
    , compile_checkPrelude =
        Check.checkDecls Check.emptyDeclEnv . fromList
    , compile_item =
        [ "table Expenses {"
        , "  id : Int, PK(id), AUTO_INCREMENT(id),"
        , "  cost : AUD"
        , "}"
        ]
    , compile_parseItem = Parser.decl
    , compile_checkItem =
      \(_, env) -> Check.checkDecls env . pure
    , compile_normalise = id
    , compile_gen =
        \_ (_, env) ->
          Right @SQL.CompileError $
          SQL.compileTable
            (Check.toLower "Expenses")
            (Maybe.fromJust . Map.lookup (Check.toLower "Expenses") $ Check._deTables env)
    , compile_output =
        Table
          "expenses"
          [ Column
            { name = "id"
            , type_ = "INTEGER"
            , notNull = True
            , autoIncrement = True
            }
          , Column
            { name = "cost_dollars"
            , type_ = "INTEGER"
            , notNull = True
            , autoIncrement = False
            }
          , Column
            { name = "cost_cents"
            , type_ = "INTEGER"
            , notNull = True
            , autoIncrement = False
            }
          ]
          []
    }
