{-# language OverloadedLists, OverloadedStrings #-}
{-# language GADTs #-}
module Main where

import Prelude hiding (Ordering(..))

import qualified Bound
import Bound.Var (unvar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Exts (fromString)
import Quill.Check (QueryEnv(..), TypeError(..), checkExpr)
import Quill.Normalise (normaliseExpr)
import Quill.Parser (Parser, parseString, decls, expr, query)
import Quill.Syntax (Decl(..), Expr(..), TableItem(..), Type(..))
import qualified Quill.Syntax as Syntax
import Test.Hspec (describe, expectationFailure, hspec, it, shouldBe)

data ConvertTest
  = ConvertTest
  { from :: Type
  , to :: Type
  , inputTerm :: Expr Void
  , outputTerm :: Expr Void
  }

convertTest :: ConvertTest -> IO ()
convertTest ct = do
  output <- either (error . show) pure $ checkExpr env (inputTerm ct) (to ct)
  let
    expected :: Expr Text
    expected = absurd <$> outputTerm ct

    actual :: Expr Text
    actual = absurd <$> output
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
  { check_type :: Type
  , check_term :: Expr Void
  }

checkTest :: CheckTest -> IO ()
checkTest ct = do
  _ <- either (error . show) pure $ checkExpr env (check_term ct) (check_type ct)
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
  { doesn'tCheck_type :: Type
  , doesn'tCheck_term :: Expr Void
  , doesn'tCheck_error :: TypeError
  }

doesn'tCheckTest :: Doesn'tCheckTest -> IO ()
doesn'tCheckTest ct = do
  case checkExpr env (doesn'tCheck_term ct) (doesn'tCheck_type ct) of
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

main :: IO ()
main =
  hspec $ do
    describe "parse" $ do
      it "1" $
        parseTest $
        ParseTest
        { parse_input =
          [ "type AUD = { dollars: Int, cents: Int };"
          ]
        , parse_parser = decls
        , parse_output =
          [ Type "AUD" $ TRecord [("dollars", TInt), ("cents", TInt)]
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
        , parse_parser = decls
        , parse_output =
          [ Table "Expenses"
            [ Field "id" TInt
            , Constraint "PK" ["id"]
            , Constraint "AUTO_INCREMENT" ["id"]
            , Field "name" $ TName "Text"
            , Field "cost" $ TName "AUD"
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
        , parse_parser = decls
        , parse_output =
          [ Query
              "selectExpensesById"
              [("id", TInt)]
              (TQuery $ TMany $ TRecord [("id", TInt), ("name", TName "Text"), ("cost", TName "AUD")])
              (Bound.toScope . Bind (SelectFrom "Expenses") "expenses" .
               Bound.toScope . Return $
               For
                 "expense"
                 (Var $ Bound.B ())
                 (Just . Bound.toScope $
                  Project (Var $ Bound.B ()) "id" `EQ` Var (Bound.F $ Bound.F $ Bound.B 0)
                 )
                 (Bound.toScope . Var $ Bound.B ())
              )
          ]
        }
    describe "convert" $ do
      it "{ x : Int, y : Bool } ==> { x : Int, y : Bool, z : Optional Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord [("x", TInt), ("y", TBool)]
        , to =
            TRecord [("x", TInt), ("y", TBool), ("z", TOptional TInt)]
        , inputTerm =
            Record [("x", Int 1), ("y", Bool True)]
        , outputTerm =
            Record [("x", Int 1), ("y", Bool True), ("z", None)]
        }
      it "{ x : Int, z : Int } ==> { x : Int, y : Optional Bool, z : Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord [("x", TInt), ("z", TInt)]
        , to =
            TRecord [("x", TInt), ("y", TOptional TBool), ("z", TInt)]
        , inputTerm =
            Record [("x", Int 1), ("z", Int 2)]
        , outputTerm =
            Record [("x", Int 1), ("y", None), ("z", Int 2)]
        }
      it "{ y : Bool, z : Int } ==> { x : Optional Int, y : Bool, z : Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord [("y", TBool), ("z", TInt)]
        , to =
            TRecord [("x", TOptional TInt), ("y", TBool), ("z", TInt)]
        , inputTerm =
            Record [("y", Bool True), ("z", Int 2)]
        , outputTerm =
            Record [("x", None), ("y", Bool True), ("z", Int 2)]
        }
      it "{ } ==> { x : Optional Int, y : Optional Bool, z : Optional Int }" $
        convertTest $
        ConvertTest
        { from =
            TRecord []
        , to =
            TRecord [("x", TOptional TInt), ("y", TOptional TBool), ("z", TOptional TInt)]
        , inputTerm =
            Record []
        , outputTerm =
            Record [("x", None), ("y", None), ("z", None)]
        }
      it "Many { x : Int } ==> Many { x : Int, y : Optional Bool }" $
        convertTest $
        ConvertTest
        { from =
            TMany $ TRecord [("x", TInt)]
        , to =
            TMany $ TRecord [("x", TInt), ("y", TOptional TBool)]
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
        , check_type = TRecord [("x", TOptional TInt)]
        }
    describe "doesn't check" $ do
      it "{ x: None, y: True } : { x : Optional Int, y : Optional Bool }" $
        doesn'tCheckTest $
        Doesn'tCheckTest
        { doesn'tCheck_term = Record [("x", None), ("y", Bool True)]
        , doesn'tCheck_type = TRecord [("x", TOptional TInt), ("y", TOptional TBool)]
        , doesn'tCheck_error = TypeMismatch (TOptional TBool) TBool
        }
