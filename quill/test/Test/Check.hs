{-# language OverloadedLists, OverloadedStrings #-}
module Test.Check (convertTests, checkTests, doesn'tCheckTests) where

import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Test.Hspec

import Quill.Check (QueryEnv(..), TypeError(..), TypeInfo(..), checkExpr, mkTypeInfo)
import Quill.Normalise (normaliseExpr)
import Quill.Syntax (Expr(..), Type(..))
import qualified Quill.Syntax as Syntax

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
    fst <$> checkExpr env (inputTerm ct) ty
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
     { _qeLanguage = Nothing
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
     { _qeLanguage = Nothing
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
     { _qeLanguage = Nothing
     , _qeNames = absurd
     , _qeLocals = absurd
     , _qeGlobalVars = mempty
     , _qeGlobalQueries = mempty
     , _qeTypes = mempty
     , _qeTables = mempty
     }

convertTests :: Spec
convertTests = do
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

checkTests :: Spec
checkTests = do
  it "{ x: None } : { x : Optional Int }" $
    checkTest $
    CheckTest
    { check_term = Record [("x", None)]
    , check_type = TRecord () [("x", TOptional () $ TInt ())]
    }

doesn'tCheckTests :: Spec
doesn'tCheckTests = do
  it "{ x: None, y: True } : { x : Optional Int, y : Optional Bool }" $
    doesn'tCheckTest $
    Doesn'tCheckTest
    { doesn'tCheck_term = Record [("x", None), ("y", Bool True)]
    , doesn'tCheck_type = TRecord () [("x", TOptional () $ TInt ()), ("y", TOptional () $ TBool ())]
    , doesn'tCheck_error = TypeMismatch (TOptional anyTypeInfo $ TBool anyTypeInfo) (TBool anyTypeInfo)
    }
