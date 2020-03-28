{-# language OverloadedLists, OverloadedStrings #-}
module Main where

import qualified Bound
import Data.Bifunctor (bimap)
import Data.Void (Void, absurd)
import Quill.Check (QueryEnv(..), convertExpr)
import Quill.Syntax (Expr, Type)
import qualified Quill.Syntax as Syntax
import Test.Hspec (describe, expectationFailure, hspec, it, shouldBe)

data ConvertTest
  = ConvertTest
  { from :: Type
  , to :: Type
  , inputTerm :: Expr Void Void
  , outputTerm :: Expr Void Void
  }

convertTest :: ConvertTest -> IO ()
convertTest ct =
  case convertExpr env (to ct) (from ct) of
    Left err -> expectationFailure $ show err
    Right f ->
      Syntax.ShowExpr (bimap absurd absurd $ Bound.instantiate1 (inputTerm ct) f) `shouldBe`
      Syntax.ShowExpr (bimap absurd absurd $ outputTerm ct)
  where
   env =
     QueryEnv
     { _qeLanguage = Syntax.SQL2003
     , _qeQueryNames = absurd
     , _qeLocalQueries = absurd
     , _qeVarNames = absurd
     , _qeLocalVars = absurd
     , _qeGlobalVars = mempty
     , _qeGlobalQueries = mempty
     , _qeTypes = mempty
     , _qeTables = mempty
     }

main :: IO ()
main =
  hspec $ do
    describe "convert" $ do
      it "{ x : Int, y : Bool } ==> { x : Int, y : Bool, z : Optional Int }" $
        convertTest $
        ConvertTest
        { from =
            Syntax.TRecord [("x", Syntax.TInt), ("y", Syntax.TBool)]
        , to =
            Syntax.TRecord [("x", Syntax.TInt), ("y", Syntax.TBool), ("z", Syntax.TOptional Syntax.TInt)]
        , inputTerm =
            Syntax.Record [("x", Syntax.Int 1), ("y", Syntax.Bool True)]
        , outputTerm =
            Syntax.Record [("x", Syntax.Int 1), ("y", Syntax.Bool True), ("z", Syntax.None)]
        }
