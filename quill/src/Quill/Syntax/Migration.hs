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

data Command
  = CreateTable
      Text
      (Vector (TableItem ()))
  | AlterTable Text FieldChange

data Migration
  = Migration
  { _mParents :: Maybe (Vector Text)
  , _mCommands :: Vector Command
  }
