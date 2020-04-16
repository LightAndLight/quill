module Quill.Syntax.Migration where

import Data.Text (Text)
import Data.Vector (Vector)
import Quill.Syntax (Constraint, TableItem, Type)

data FieldChange typeInfo
  = DropField Text
  -- | Alter
  | AddField Text (Type typeInfo)
  | AddConstraint
      Constraint
      (Vector Text)
  deriving (Eq, Show)

data Command typeInfo
  = CreateTable
      Text
      (Vector (TableItem typeInfo))
  | AlterTable Text (Vector (FieldChange typeInfo))
  deriving (Eq, Show)

newtype Name = Name Text deriving (Eq, Ord, Show)

data Migration typeInfo
  = Migration
  { _mName :: Name
  , _mParent :: Maybe Name
  , _mCommands :: Vector (Command typeInfo)
  } deriving (Eq, Show)
