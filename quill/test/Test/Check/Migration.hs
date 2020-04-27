{-# language OverloadedLists, OverloadedStrings #-}
module Test.Check.Migration (checkTests) where

import Control.Applicative (some)
import qualified Data.Vector as Vector
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Quill.Check (TypeInfo(..))
import Quill.Check.Migration (MigrationEnv(..), MigrationError)
import qualified Quill.Check as Check (Origin(..), TableError(..), ConstraintError(..))
import qualified Quill.Check.Migration as Check (MigrationError(..), checkMigrations, emptyMigrationEnv)
import Quill.Syntax (Constraint(..), TableItem(..), Type(..))
import Quill.Syntax.Migration (Migration(..))
import qualified Quill.Syntax.Migration as Migration
import qualified Quill.Parser as Parser (eof, parseString)
import qualified Quill.Parser.Migration as Parser (migration)

data Check
  = Check
  { _check_input :: [String]
  , _check_root :: Migration.Name
  , _check_outcome :: Either (MigrationError () ()) MigrationEnv -> IO ()
  }

check :: Check -> IO ()
check args =
  case Parser.parseString (some Parser.migration <* Parser.eof) (unlines $ _check_input args) of
    Left err -> expectationFailure err
    Right ms ->
      _check_outcome args $
      Check.checkMigrations
        (Vector.fromList ms)
        Check.emptyMigrationEnv
        (_check_root args)

checkTests :: Spec
checkTests = do
  describe "failure" failureTests
  describe "success" successTests

failureTests :: Spec
failureTests = do
  it "duplicate migration" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: []"
      , "}"
      , ""
      , "migration \"a\" {"
      , "  parent: \"a\","
      , "  commands: []"
      , "}"
      ]
    , _check_root = Migration.Name "a"
    , _check_outcome = \output -> output `shouldBe` Left (Check.DuplicateMigration $ Migration.Name "a")
    }
  it "migration not found" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: []"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"a\","
      , "  commands: []"
      , "}"
      ]
    , _check_root = Migration.Name "c"
    , _check_outcome = \output -> output `shouldBe` Left (Check.MigrationNotFound $ Migration.Name "c")
    }
  it "cyclic migration" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: []"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"b\","
      , "  commands: []"
      , "}"
      ]
    , _check_root = Migration.Name "b"
    , _check_outcome = \output -> output `shouldBe` Left (Check.CycleDetected [Migration.Name "b"])
    }
  it "longer cyclic migration" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: []"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"d\","
      , "  commands: []"
      , "}"
      , ""
      , "migration \"c\" {"
      , "  parent: \"b\","
      , "  commands: []"
      , "}"
      , ""
      , "migration \"d\" {"
      , "  parent: \"c\","
      , "  commands: []"
      , "}"
      ]
    , _check_root = Migration.Name "d"
    , _check_outcome =
      \output ->
        output `shouldBe`
        Left (Check.CycleDetected [Migration.Name "d", Migration.Name "c", Migration.Name "b"])
    }
  it "table already exists" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: ["
      , "    create table Table1 {"
      , "      a : Int"
      , "    }"
      , "  ]"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"a\","
      , "  commands: ["
      , "    create table Table1 {"
      , "      b : Bool"
      , "    }"
      , "  ]"
      , "}"
      ]
    , _check_root = Migration.Name "b"
    , _check_outcome =
      \output ->
        output `shouldBe`
        Left (Check.MigrationTableError $ Check.TableAlreadyDefined "Table1")
    }
  it "table not found" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: ["
      , "    create table Table1 {"
      , "      a : Int"
      , "    }"
      , "  ]"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"a\","
      , "  commands: ["
      , "    alter table Table2 {"
      , "      add b : Bool"
      , "    }"
      , "  ]"
      , "}"
      ]
    , _check_root = Migration.Name "b"
    , _check_outcome =
      \output ->
        output `shouldBe`
        Left (Check.TableNotFound "Table2")
    }
  it "table not found" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: ["
      , "    create table Table1 {"
      , "      a : Int"
      , "    }"
      , "  ]"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"a\","
      , "  commands: ["
      , "    alter table Table1 {"
      , "      drop b"
      , "    }"
      , "  ]"
      , "}"
      ]
    , _check_root = Migration.Name "b"
    , _check_outcome =
      \output ->
        output `shouldBe`
        Left (Check.FieldNotFound "b")
    }
  it "field already exists" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: ["
      , "    create table Table1 {"
      , "      a : Int"
      , "    }"
      , "  ]"
      , "}"
      , ""
      , "migration \"b\" {"
      , "  parent: \"a\","
      , "  commands: ["
      , "    alter table Table1 {"
      , "      add a : Int"
      , "    }"
      , "  ]"
      , "}"
      ]
    , _check_root = Migration.Name "b"
    , _check_outcome =
      \output ->
        output `shouldBe`
        Left (Check.FieldAlreadyExists "a")
    }
  describe "constraint error" $ do
    it "arg mismatch" $
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      a : Int"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      add constraint AUTO_INCREMENT(a, b)"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = Migration.Name "b"
      , _check_outcome =
        \output ->
          output `shouldBe`
          Left (Check.MigrationConstraintError $ Check.ConstraintArgsMismatch 1 2)
      }
    it "field not in scope" $
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      a : Int"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      add constraint PK(a, b)"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = Migration.Name "b"
      , _check_outcome =
        \output ->
          output `shouldBe`
          Left (Check.MigrationConstraintError $ Check.FieldNotInScope "b")
      }
    it "unknown constraint" $
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      a : Int"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      add constraint Blaaah(a)"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = Migration.Name "b"
      , _check_outcome =
        \output ->
          output `shouldBe`
          Left (Check.MigrationConstraintError $ Check.UnknownConstraint "Blaaah")
      }
    it "not enumerable" $
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      a : Bool"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      add constraint AUTO_INCREMENT(a)"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = Migration.Name "b"
      , _check_outcome =
        \output ->
          output `shouldBe`
          Left
            (Check.MigrationConstraintError .
             Check.NotEnumerable $
             TBool (TypeInfo { _typeInfoOrigin = Just Check.Column })
            )
      }
    it "multiple primary keys" $
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      a : Int, PK(a)"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      add b : Int,"
        , "      add constraint PK(b)"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = Migration.Name "b"
      , _check_outcome =
        \output ->
          output `shouldBe`
          Left (Check.MigrationConstraintError $ Check.MultiplePrimaryKeys "Table1")
      }

successTests :: Spec
successTests = do
  it "initial migration - empty" $
    check $
    Check
    { _check_input =
      [ "initial migration \"a\" {"
      , "  commands: []"
      , "}"
      ]
    , _check_root = Migration.Name "a"
    , _check_outcome =
      let
        m_a = Migration.Name "a"
      in
      \output ->
        output `shouldBe`
        Right
          (Check.emptyMigrationEnv
           { _meMigrations = [(m_a, Migration m_a Nothing [])]
           }
          )
    }
  it "initial migration - create table" $
    let
      m_a = Migration.Name "a"
    in
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      id : Int, PK(id),"
        , "      b : Bool"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = m_a
      , _check_outcome =
        \output ->
          let
            idTy = TInt (TypeInfo { _typeInfoOrigin = Just Check.Column })
            bTy = TBool (TypeInfo { _typeInfoOrigin = Just Check.Column })
          in
            _meMigrations <$> output `shouldBe`
            Right
            [ ( m_a
              , Migration
                  m_a
                  Nothing
                  [ Migration.CreateTable "Table1"
                    [ Field "id" idTy
                    , Constraint PrimaryKey ["id"]
                    , Field "b" bTy
                    ]
                  ]
              )
            ]
        }
  it "initial migration - create and alter" $
    let
      m_a = Migration.Name "a"
    in
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      id : Int, PK(id),"
        , "      b : Bool"
        , "    },"
        , "    alter table Table1 {"
        , "      drop b"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = m_a
      , _check_outcome =
        \output ->
          _meMigrations <$> output `shouldBe`
          Right
          [ ( m_a
            , Migration m_a Nothing
              [ Migration.CreateTable "Table1"
                [ Field "id" $ TInt (TypeInfo { _typeInfoOrigin = Just Check.Column })
                , Constraint PrimaryKey ["id"]
                , Field "b" $ TBool (TypeInfo { _typeInfoOrigin = Just Check.Column })
                ]
              , Migration.AlterTable "Table1" [Migration.DropField "b"]
              ]
            )
          ]
      }
  it "migration - create and alter" $
    let
      m_a = Migration.Name "a"
      m_b = Migration.Name "b"
    in
      check $
      Check
      { _check_input =
        [ "initial migration \"a\" {"
        , "  commands: ["
        , "    create table Table1 {"
        , "      id : Int, PK(id),"
        , "      b : Bool"
        , "    }"
        , "  ]"
        , "}"
        , ""
        , "migration \"b\" {"
        , "  parent: \"a\","
        , "  commands: ["
        , "    alter table Table1 {"
        , "      drop b"
        , "    }"
        , "  ]"
        , "}"
        ]
      , _check_root = m_b
      , _check_outcome =
        \output ->
          _meMigrations <$> output `shouldBe`
          Right
          [ ( m_a
            , Migration m_a Nothing
              [ Migration.CreateTable "Table1"
                [ Field "id" $ TInt (TypeInfo { _typeInfoOrigin = Just Check.Column })
                , Constraint PrimaryKey ["id"]
                , Field "b" $ TBool (TypeInfo { _typeInfoOrigin = Just Check.Column })
                ]
              ]
            )
          , ( m_b
            , Migration m_b (Just m_a) [Migration.AlterTable "Table1" [Migration.DropField "b"]]
            )
          ]
      }
