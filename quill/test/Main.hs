{-# language OverloadedLists, OverloadedStrings #-}
{-# language GADTs #-}
module Main where

import Prelude hiding (Ordering(..))

import qualified Bound
import Bound.Var (unvar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Exts (fromList, fromString)
import Quill.Check
  ( DeclEnv(..), QueryEnv(..), TypeError(..), TypeInfo(..)
  , checkExpr, mkTypeInfo
  )
import qualified Quill.Check as Check
import Quill.Normalise (normaliseExpr)
import Quill.Parser (Parser, eof, parseString, decls, expr, query)
import Quill.Syntax (Decl(..), Expr(..), TableItem(..), Type(..))
import qualified Quill.Syntax as Syntax
import qualified Quill.SQL as SQL
import Test.Hspec (describe, expectationFailure, hspec, it, shouldBe)

anyTypeInfo :: TypeInfo
anyTypeInfo = TypeInfo { _typeInfoOrigin = Nothing }

data ConvertTest
  = ConvertTest
  { from :: Type ()
  , to :: Type ()
  , inputTerm :: Expr () Void
  , outputTerm :: Expr () Void
  }

convertTest :: ConvertTest -> IO ()
convertTest ct = do
  output <- either (error . show) pure $ do
    ty <- mkTypeInfo env Nothing (to ct)
    checkExpr env (inputTerm ct) ty
  let
    expected :: Expr () Text
    expected = bimap (\_ -> ()) absurd $ outputTerm ct

    actual :: Expr () Text
    actual = bimap (\_ -> ()) absurd $ output
  -- print $ Syntax.ShowExpr actual
  normaliseExpr actual `shouldBe` normaliseExpr expected
  where
   env =
     QueryEnv
     { _qeLanguage = Syntax.SQL2003
     , _qeNames = absurd
     , _qeLocals = absurd
     , _qeGlobalVars = mempty
     , _qeGlobalQueries = mempty
     , _qeTypes = mempty
     , _qeTables = mempty
     }

data CheckTest
  = CheckTest
  { check_type :: Type ()
  , check_term :: Expr () Void
  }

checkTest :: CheckTest -> IO ()
checkTest ct = do
  _ <- either (error . show) pure $ do
    ty <- mkTypeInfo env Nothing (check_type ct)
    checkExpr env (check_term ct) ty
  pure ()
  where
   env =
     QueryEnv
     { _qeLanguage = Syntax.SQL2003
     , _qeNames = absurd
     , _qeLocals = absurd
     , _qeGlobalVars = mempty
     , _qeGlobalQueries = mempty
     , _qeTypes = mempty
     , _qeTables = mempty
     }

data Doesn'tCheckTest
  = Doesn'tCheckTest
  { doesn'tCheck_type :: Type ()
  , doesn'tCheck_term :: Expr () Void
  , doesn'tCheck_error :: TypeError ()
  }

doesn'tCheckTest :: Doesn'tCheckTest -> IO ()
doesn'tCheckTest ct = do
  let
    m = do
      ty <- mkTypeInfo env Nothing (doesn'tCheck_type ct)
      checkExpr env (doesn'tCheck_term ct) ty
  case m of
    Left err -> err `shouldBe` doesn'tCheck_error ct
    Right a -> expectationFailure "\"doesn't check\" test failed"
  where
   env =
     QueryEnv
     { _qeLanguage = Syntax.SQL2003
     , _qeNames = absurd
     , _qeLocals = absurd
     , _qeGlobalVars = mempty
     , _qeGlobalQueries = mempty
     , _qeTypes = mempty
     , _qeTables = mempty
     }

data ParseTest where
  ParseTest ::
    (Eq a, Show a) =>
    { parse_input :: [String]
    , parse_parser :: Parser a
    , parse_output :: a
    } ->
    ParseTest

parseTest :: ParseTest -> IO ()
parseTest (ParseTest { parse_input = input, parse_parser = p, parse_output = output }) =
  case parseString p (unlines input) of
    Left err -> expectationFailure err
    Right a -> a `shouldBe` output

data CompileTest where
  CompileTest ::
    (Show e, Show e', Show item') =>
    { compile_prelude :: [String]
    , compile_parsePrelude :: Parser prelude
    , compile_checkPrelude :: prelude -> Either e prelude'
    , compile_item :: [String]
    , compile_parseItem :: Parser item
    , compile_checkItem :: prelude' -> item -> Either e' item'
    , compile_normalise :: item' -> item'
    , compile_gen :: prelude' -> item' -> ByteString
    , compile_output :: ByteString
    } ->
    CompileTest

compileTest :: CompileTest -> IO ()
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
  case parseString parsePrelude (unlines prelude) of
    Left err -> expectationFailure err
    Right prelude ->
      case checkPrelude prelude of
        Left err -> expectationFailure $ show err
        Right prelude' ->
          case parseString parseItem (unlines item) of
            Left err -> expectationFailure err
            Right item ->
              case checkItem prelude' item of
                Left err -> expectationFailure $ show err
                Right item' -> do
                  -- print (normalise item')
                  gen prelude' (normalise item') `shouldBe` output

main :: IO ()
main =
  hspec $ do
    describe "compile" $ do
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
        , compile_parsePrelude = decls
        , compile_checkPrelude =
            Check.checkDecls (Check.emptyDeclEnv Syntax.SQL2003) . fromList
        , compile_item =
            [ "expenses <- select from Expenses;"
            , "return ("
            , "  for expense in expenses"
            , "  yield expense.cost.dollars"
            , ")"
            ]
        , compile_parseItem = query (const Nothing)
        , compile_checkItem =
          \env e ->
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
              Check.checkExpr queryEnv e $
              TQuery anyTypeInfo (TMany anyTypeInfo $ TInt anyTypeInfo)
        , compile_normalise = normaliseExpr
        , compile_gen =
          \_ e ->
            Lazy.toStrict . Builder.toLazyByteString $
            SQL.expr absurd e
        , compile_output = "SELECT expense.cost_dollars FROM ((SELECT * FROM Expenses)) AS expense"
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
        , compile_parsePrelude = decls
        , compile_checkPrelude =
            Check.checkDecls (Check.emptyDeclEnv Syntax.SQL2003) . fromList
        , compile_item =
            [ "expenses <- select from Expenses;"
            , "return ("
            , "  for expense in expenses"
            , "  yield expense.cost"
            , ")"
            ]
        , compile_parseItem = query (const Nothing)
        , compile_checkItem =
          \env e ->
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
              Check.checkExpr queryEnv e $
              TQuery anyTypeInfo (TMany anyTypeInfo $ TName anyTypeInfo "AUD")
        , compile_normalise = normaliseExpr
        , compile_gen =
          \_ e ->
            Lazy.toStrict . Builder.toLazyByteString $
            SQL.expr absurd e
        , compile_output = "SELECT expense.cost_dollars, expense.cost_cents FROM ((SELECT * FROM Expenses)) AS expense"
        }
    describe "parse" $ do
      it "1" $
        parseTest $
        ParseTest
        { parse_input =
          [ "type AUD = { dollars: Int, cents: Int };"
          ]
        , parse_parser = decls :: Parser [Decl () ()]
        , parse_output =
          [ Type "AUD" $ TRecord () [("dollars", TInt ()), ("cents", TInt ())]
          ]
        }
      it "2" $
        parseTest $
        ParseTest
        { parse_input =
          [ "table Expenses {"
          , "  id : Int, PK(id), AUTO_INCREMENT(id),"
          , "  name : Text,"
          , "  cost : AUD"
          , "}"
          ]
        , parse_parser = decls :: Parser [Decl () ()]
        , parse_output =
          [ Table "Expenses"
            [ Field "id" $ TInt ()
            , Constraint "PK" ["id"]
            , Constraint "AUTO_INCREMENT" ["id"]
            , Field "name" $ TName () "Text"
            , Field "cost" $ TName () "AUD"
            ]
          ]
        }
      it "3" $
        parseTest $
        ParseTest
        { parse_input =
          [ "query selectExpensesById(id: Int) -> Query (Many { id : Int, name : Text, cost : AUD }) {"
          , "  expenses <- select from Expenses;"
          , "  return ("
          , "    for expense in expenses"
          , "    where expense.id == id"
          , "    yield expense"
          , "  )"
          , "}"
          ]
        , parse_parser = decls :: Parser [Decl () ()]
        , parse_output =
          [ Query
              "selectExpensesById"
              [("id", TInt ())]
              (TQuery () $ TMany () $
               TRecord () [("id", TInt ()), ("name", TName () "Text"), ("cost", TName () "AUD")]
              )
              (Bound.toScope . Bind (SelectFrom "Expenses") "expenses" .
               Syntax.toScope2 . Return $
               For
                 "expense"
                 (Var $ Bound.B ())
                 (Just . Syntax.toScope2 $
                  Project (Var $ Bound.B ()) "id" `EQ` Var (Bound.F $ Bound.F $ Bound.B 0)
                 )
                 (Syntax.toScope2 . Var $ Bound.B ())
              )
          ]
        }
      it "4" $
        parseTest $
        ParseTest
        { parse_input =
          [ "a.b.c" ]
        , parse_parser = (expr (const Nothing) <* eof) :: Parser (Expr () Void)
        , parse_output = Project (Project (Name "a") "b") "c"
        }
    describe "convert" $ do
      it "{ x : Int, y : Bool } ==> { x : Int, y : Bool, z : Optional Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord () [("x", TInt ()), ("y", TBool ())]
        , to =
            TRecord () [("x", TInt ()), ("y", TBool ()), ("z", TOptional () $ TInt ())]
        , inputTerm =
            Record [("x", Int 1), ("y", Bool True)]
        , outputTerm =
            Record
            [ ("x", Int 1)
            , ("y", Bool True)
            , ("z", None)
            ]
        }
      it "{ x : Int, z : Int } ==> { x : Int, y : Optional Bool, z : Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord () [("x", TInt ()), ("z", TInt ())]
        , to =
            TRecord () [("x", TInt ()), ("y", TOptional () $TBool ()), ("z", TInt ())]
        , inputTerm =
            Record [("x", Int 1), ("z", Int 2)]
        , outputTerm =
            Record [("x", Int 1), ("y", None), ("z", Int 2)]
        }
      it "{ y : Bool, z : Int } ==> { x : Optional Int, y : Bool, z : Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord () [("y", TBool ()), ("z", TInt ())]
        , to =
            TRecord () [("x", TOptional () $ TInt ()), ("y", TBool ()), ("z", TInt ())]
        , inputTerm =
            Record [("y", Bool True), ("z", Int 2)]
        , outputTerm =
            Record [("x", None), ("y", Bool True), ("z", Int 2)]
        }
      it "{ } ==> { x : Optional Int, y : Optional Bool, z : Optional Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord () []
        , to =
            TRecord ()
            [ ("x", TOptional () $ TInt ())
            , ("y", TOptional () $ TBool ())
            , ("z", TOptional () $ TInt ())
            ]
        , inputTerm =
            Record []
        , outputTerm =
            Record [("x", None), ("y", None), ("z", None)]
        }
      it "Many { x : Int } ==> Many { x : Int, y : Optional Bool }" $
        convertTest $
        ConvertTest
        { from =
            TMany () $ TRecord () [("x", TInt ())]
        , to =
            TMany () $ TRecord () [("x", TInt ()), ("y", TOptional () $ TBool ())]
        , inputTerm =
            Many
            [ Record [("x", Int 0)]
            , Record [("x", Int 1), ("y", Some $ Bool False)]
            , Record [("x", Int 2)]
            , Record [("x", Int 3), ("y", Some $ Bool True)]
            , Record [("x", Int 4), ("y", None)]
            ]
        , outputTerm =
            Many
            [ Record [("x", Int 0), ("y", None)]
            , Record [("x", Int 1), ("y", Some $ Bool False)]
            , Record [("x", Int 2), ("y", None)]
            , Record [("x", Int 3), ("y", Some $ Bool True)]
            , Record [("x", Int 4), ("y", None)]
            ]
        }
    describe "check" $ do
      it "{ x: None } : { x : Optional Int }" $
        checkTest $
        CheckTest
        { check_term = Record [("x", None)]
        , check_type = TRecord () [("x", TOptional () $ TInt ())]
        }
    describe "doesn't check" $ do
      it "{ x: None, y: True } : { x : Optional Int, y : Optional Bool }" $
        doesn'tCheckTest $
        Doesn'tCheckTest
        { doesn'tCheck_term = Record [("x", None), ("y", Bool True)]
        , doesn'tCheck_type = TRecord () [("x", TOptional () $ TInt ()), ("y", TOptional () $ TBool ())]
        , doesn'tCheck_error = TypeMismatch (TOptional anyTypeInfo $ TBool anyTypeInfo) (TBool anyTypeInfo)
        }
