{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
module Quill.Check.Migration
  ( MigrationError(..)
  , MigrationEnv
  , emptyMigrationEnv
  , checkMigration
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)

import Quill.Syntax (Constraint, Type)
import qualified Quill.Syntax as Syntax
import Quill.Syntax.Migration (Command(..), Migration(..))
import qualified Quill.Syntax.Migration as Migration (Name(..))

data Tracked a = Tracked { _origin :: Migration.Name, _data :: a }
  deriving (Functor, Foldable, Traversable)

data TableItem
  = Field Text (Tracked (Type ()))
  | Constraint Constraint (Vector Text)

newtype Table = Table { unTable :: Vector (Tracked TableItem) }

data MigrationEnv
  = MigrationEnv
  { _meNames :: Set Migration.Name
  , _meTables :: Map Text (Tracked Table)
  }

emptyMigrationEnv :: MigrationEnv
emptyMigrationEnv =
  MigrationEnv
  { _meNames = mempty
  , _meTables = mempty
  }

data MigrationError
  = DuplicateMigration Migration.Name
  | TableAlreadyExists Text
  | TableDoesn'tExist Text

checkCommand ::
  MonadError MigrationError m =>
  Migration.Name ->
  MigrationEnv ->
  Command ->
  m MigrationEnv
checkCommand m env c =
  case c of
    CreateTable tableName fields -> do
      when (Map.member tableName $ _meTables env) . throwError $ TableAlreadyExists tableName
      let
        table :: Table
        table =
          Table $
          Tracked m .
          (\case
             Syntax.Field fieldName fieldType ->
               Field fieldName (Tracked m fieldType)
             Syntax.Constraint constr args ->
               Constraint constr args
          ) <$>
          fields
      pure $ env { _meTables = Map.insert tableName (Tracked m table) $ _meTables env }
    AlterTable tableName changes -> do
      entry <- maybe (throwError $ TableDoesn'tExist tableName) pure $ Map.lookup tableName (_meTables env)
      entry' <-
        for entry $ \entry' -> do
          let items = unTable entry'
          items' <- _ items
          pure $ Table items'
      pure $ env { _meTables = Map.insert tableName entry' $ _meTables env }

checkMigration ::
  MonadError MigrationError m =>
  (MigrationEnv -> Migration.Name -> m MigrationEnv) ->
  MigrationEnv ->
  Migration ->
  m MigrationEnv
checkMigration check env (Migration name m_parents commands) = do
  when (Set.member name $ _meNames env) . throwError $ DuplicateMigration name
  env' <-
    case m_parents of
      Nothing -> pure env
      Just parents -> foldlM check env parents
  foldlM (checkCommand name) env' commands
