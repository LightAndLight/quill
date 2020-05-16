{-# language ViewPatterns #-}
module Quill.SQL.Migration
  (compileName, compileHash, compileMigration)
where

import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Capnp.Gen.Migration.Pure as SQL
  ( AlterTable(AlterTable)
  , Command(..)
  , Migration(..), Migration'parent(..)
  , ParentInfo(ParentInfo)
  , TableChange(..)
  )
import qualified Capnp.Gen.Table.Pure as SQL
  (Column(Column), Constraint(..), Other(Other))
import Quill.Check (TypeInfo, toLower, unLowercase)
import Quill.Check.Migration (MigrationEnv(..), MigrationInfo(..))
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
    CreateTable (toLower -> tableName) _ ->
      case Map.lookup tableName (_meTables env) of
        Nothing -> error "impossible: table not found"
        Just info ->
          SQL.Command'createTable $
          compileTable tableName info
    AlterTable (toLower -> tableName) changes ->
      SQL.Command'alterTable $
      SQL.AlterTable
        (encodeUtf8 $ unLowercase tableName)
        (compileChange <$> changes)

compileHash :: Int -> ByteString
compileHash = Base64.encode . Lazy.toStrict . Binary.encode

compileMigration :: MigrationEnv -> Migration MigrationInfo TypeInfo -> SQL.Migration
compileMigration env (Migration name m_parent commands info) =
  SQL.Migration
  { SQL.name = compileName name
  , SQL.hash = compileHash $ _miHash info
  , SQL.parent =
      case m_parent of
        Nothing -> SQL.Migration'parent'none
        Just parent ->
          case _miParentHash info of
            Nothing -> error $ "internal error: missing parent hash for " <> show parent
            Just parentHash ->
              SQL.Migration'parent'some $
              SQL.ParentInfo
                (compileName parent)
                (compileHash parentHash)
  , SQL.commands = compileCommand env <$> commands
  }
