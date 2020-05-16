module Quill.Syntax.Migration where

import Data.Hashable (hashWithSalt)
import Data.Hashable.Lifted (liftHashWithSalt)
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Hashable.Lifted.Orphans ()
import Quill.Syntax (Constraint, TableItem, Type, hashConstraintWithSalt, hashTableItemWithSalt, hashTypeWithSalt)

data FieldChange typeInfo
  = DropField Text
  -- | Alter
  | AddField Text (Type typeInfo)
  | AddConstraint
      Constraint
      (Vector Text)
  deriving (Eq, Show)

hashFieldChangeWithSalt :: Int -> FieldChange typeInfo -> Int
hashFieldChangeWithSalt s f =
  case f of
    DropField fieldName ->
      s `hashWithSalt` (0::Int) `hashWithSalt` fieldName
    AddField fieldName fieldType ->
      hashTypeWithSalt
        (s `hashWithSalt` (1::Int) `hashWithSalt` fieldName)
        fieldType
    AddConstraint constr args ->
      liftHashWithSalt
        hashWithSalt
        (hashConstraintWithSalt
           (s `hashWithSalt` (2::Int))
           constr
        )
        args

data Command typeInfo
  = CreateTable
      Text
      (Vector (TableItem typeInfo))
  | AlterTable Text (Vector (FieldChange typeInfo))
  deriving (Eq, Show)

hashCommandWithSalt :: Int -> Command typeInfo -> Int
hashCommandWithSalt s c =
  case c of
    CreateTable tableName tableItems ->
      liftHashWithSalt
        hashTableItemWithSalt
        (s `hashWithSalt` (0::Int) `hashWithSalt` tableName)
        tableItems
    AlterTable tableName fieldChanges ->
      liftHashWithSalt
        hashFieldChangeWithSalt
        (s `hashWithSalt` (1::Int) `hashWithSalt` tableName)
        fieldChanges

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

data Migration migrInfo typeInfo
  = Migration
  { _mName :: Name
  , _mParent :: Maybe Name
  , _mCommands :: Vector (Command typeInfo)
  , _mInfo :: migrInfo
  } deriving (Eq, Show)
