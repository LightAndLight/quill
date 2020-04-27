{-# language OverloadedLists, OverloadedStrings #-}
module Test.Check.Migration (checkTests) where

import Control.Applicative (some)
import qualified Data.Vector as Vector
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Quill.Check.Migration (MigrationEnv(..), MigrationError)
import qualified Quill.Check as Check (TableError(..), ConstraintError(..))
import qualified Quill.Check.Migration as Check
import Quill.Syntax (Constraint(..), Type(..))
import qualified Quill.Syntax.Migration as Migration
import qualified Quill.Parser as Parser (eof, parseString)
import qualified Quill.Parser.Migration as Parser (migration)

data Check
  = Check
  { _check_input :: [String]
  , _check_root :: Migration.Name
  , _check_outcome :: Either MigrationError MigrationEnv -> IO ()
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
        Left (Check.TableAlreadyDefined "Table1")
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
          Left (Check.TableConstraintError $ Check.FieldNotInScope "b")
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
          Left (Check.TableConstraintError $ Check.UnknownConstraint "Blaaah")
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
          Left (Check.TableConstraintError . Check.NotEnumerable $ TBool ())
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
          Left (Check.TableConstraintError $ Check.MultiplePrimaryKeys "Table1")
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
      \output ->
        output `shouldBe`
        Right
          (Check.emptyMigrationEnv
           { _meChecked = [Migration.Name "a"]}
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
          output `shouldBe`
          Right
            (Check.emptyMigrationEnv
            { _meChecked = [m_a]
            , _meTables =
              [ ( "Table1"
                , Check.Tracked m_a $
                  [ Tracked m_a $ Field "id" (Tracked m_a $ TInt ())
                  , Tracked m_a $ Constraint PrimaryKey ["id"]
                  , Tracked m_a $ Field "b" (Tracked m_a $ TBool ())
                  ]
                )
              ]
            }
            )
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
          output `shouldBe`
          Right
            (Check.emptyMigrationEnv
            { _meChecked = [m_a]
            , _meTables =
              [ ( "Table1"
                , Check.Tracked m_a $
                  [ Tracked m_a $ Field "id" (Tracked m_a $ TInt ())
                  , Tracked m_a $ Constraint PrimaryKey ["id"]
                  ]
                )
              ]
            }
            )
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
          output `shouldBe`
          Right
            (Check.emptyMigrationEnv
            { _meChecked = [m_a, m_b]
            , _meTables =
              [ ( "Table1"
                , Check.Tracked m_b $
                  [ Tracked m_a $ Field "id" (Tracked m_a $ TInt ())
                  , Tracked m_a $ Constraint PrimaryKey ["id"]
                  ]
                )
              ]
            }
            )
      }
