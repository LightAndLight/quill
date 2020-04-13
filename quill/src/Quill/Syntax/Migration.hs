module Quill.Syntax.Migration where

import Data.Text (Text)
import Data.Vector (Vector)
import Quill.Syntax (Constraint, TableItem, Type)

data FieldChange
  = DropField Text
  -- | Alter
  | AddField Text (Type ())
  | AddConstraint
      Constraint
      (Vector Text)
  deriving (Eq, Show)

data Command
  = CreateTable
      Text
      (Vector (TableItem ()))
  | AlterTable Text (Vector FieldChange)
  deriving (Eq, Show)

newtype Name = Name Text deriving (Eq, Ord, Show)

data Migration
  = Migration
  { _mName :: Name
  , _mParents :: Maybe (Vector Name)
  , _mCommands :: Vector Command
  } deriving (Eq, Show)
