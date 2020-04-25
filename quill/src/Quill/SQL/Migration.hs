module Quill.SQL.Migration (compileMigration) where

import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Capnp.Gen.Migration.Pure as SQL
  ( AlterTable(AlterTable)
  , Command(..)
  , Migration(..), Migration'parent(..)
  , TableChange(..)
  )
import qualified Capnp.Gen.Table.Pure as SQL
  (Column(Column), Constraint(..), Other(Other))
import Quill.Check (TypeInfo)
import Quill.Check.Migration (MigrationEnv(..))
import Quill.SQL (compileTable, compileType)
import Quill.Syntax (Constraint(..))
import Quill.Syntax.Migration (Command(..), FieldChange(..), Migration(..))
import qualified Quill.Syntax.Migration as Migration

compileName :: Migration.Name -> ByteString
compileName = encodeUtf8 . Migration.unName

compileConstraint :: Constraint -> Vector Text -> SQL.Constraint
compileConstraint constraint args =
  case constraint of
    AutoIncrement ->
      SQL.Constraint'autoIncrement $
      encodeUtf8 (args Vector.! 0)
    PrimaryKey ->
      SQL.Constraint'primaryKey $
      encodeUtf8 <$> args
    Other name ->
      SQL.Constraint'other $
      SQL.Other
        (encodeUtf8 name)
        (encodeUtf8 <$> args)

compileChange :: FieldChange TypeInfo -> SQL.TableChange
compileChange change =
  case change of
    DropField fieldName ->
      SQL.TableChange'dropColumn $ encodeUtf8 fieldName
    AddField fieldName fieldType ->
      SQL.TableChange'addColumn $
      SQL.Column
        (encodeUtf8 fieldName)
        (compileType fieldType)
        True
        False
    AddConstraint constraint args ->
      SQL.TableChange'addConstraint $
      compileConstraint constraint args

compileCommand :: MigrationEnv -> Command TypeInfo -> SQL.Command
compileCommand env cmd =
  case cmd of
    CreateTable tableName _ ->
      case Map.lookup tableName (_meTables env) of
        Nothing -> error "impossible: table not found"
        Just info ->
          SQL.Command'createTable $
          compileTable tableName info
    AlterTable tableName changes ->
      SQL.Command'alterTable $
      SQL.AlterTable
        (encodeUtf8 tableName)
        (compileChange <$> changes)

compileMigration :: MigrationEnv -> Migration TypeInfo -> SQL.Migration
compileMigration env (Migration name m_parent commands) =
  SQL.Migration
  { SQL.name = compileName name
  , SQL.parent =
      maybe
        SQL.Migration'parent'none
        (SQL.Migration'parent'some . compileName)
        m_parent
  , SQL.commands = compileCommand env <$> commands
  }
