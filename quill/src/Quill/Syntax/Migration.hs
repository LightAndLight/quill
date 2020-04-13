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

data Migration
  = Migration
  { _mName :: Text
  , _mParents :: Maybe (Vector Text)
  , _mCommands :: Vector Command
  } deriving (Eq, Show)
