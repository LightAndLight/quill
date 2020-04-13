{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Quill.Check.Migration
  ( MigrationError(..)
  , MigrationEnv
  , emptyMigrationEnv
  , checkMigration
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (foldl', foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Quill.Check (ConstraintError, checkConstraint, mapError)
import Quill.Syntax (Constraint(..), Type)
import qualified Quill.Syntax as Syntax
import Quill.Syntax.Migration (Command(..), FieldChange(..), Migration(..))
import qualified Quill.Syntax.Migration as Migration (Name(..))

data Tracked a = Tracked { origin :: Migration.Name, unTracked :: a }
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
  | FieldNotFound Text
  | FieldAlreadyExists Text
  | ConstraintError (ConstraintError ())
  deriving Show

checkCommand ::
  forall m.
  MonadError MigrationError m =>
  Migration.Name ->
  MigrationEnv ->
  Command ->
  m MigrationEnv
checkCommand currentMigration env c =
  case c of
    CreateTable tableName fields -> do
      when (Map.member tableName $ _meTables env) . throwError $ TableAlreadyExists tableName
      let
        table :: Table
        table =
          Table $
          track .
          (\case
             Syntax.Field fieldName fieldType ->
               Field fieldName (track fieldType)
             Syntax.Constraint constr args ->
               Constraint constr args
          ) <$>
          fields
      pure $ env { _meTables = Map.insert tableName (track table) $ _meTables env }
    AlterTable tableName changes -> do
      entry <- maybe (throwError $ TableDoesn'tExist tableName) pure $ Map.lookup tableName (_meTables env)
      entry' <- for entry $ \entry' -> foldlM (runChange tableName) entry' changes
      pure $ env { _meTables = Map.insert tableName entry' $ _meTables env }
  where
    track x = Tracked { origin = currentMigration, unTracked = x }

    dropField ::
      Table ->
      Text ->
      m Table
    dropField table fieldName =
      let
        cond = (\case; Field n _ -> n == fieldName; _ -> False) . unTracked
      in
        case Vector.find cond (unTable table) of
          Nothing -> throwError $ FieldNotFound fieldName
          Just{} -> pure . Table $ Vector.filter (not . cond) (unTable table)

    addField ::
      Table ->
      Text ->
      Type () ->
      m Table
    addField table fieldName fieldType =
      let
        cond = (\case; Field n _ -> n == fieldName; _ -> False) . unTracked
      in
        case Vector.find cond (unTable table) of
          Just{} -> throwError $ FieldAlreadyExists fieldName
          Nothing ->
            pure . Table $
            Vector.snoc
              (unTable table)
              (Tracked currentMigration $
               Field fieldName (Tracked currentMigration fieldType)
              )

    addConstraint ::
      Text ->
      Table ->
      Constraint ->
      Vector Text ->
      m Table
    addConstraint tableName table constr args = do
      mapError ConstraintError $
        checkConstraint
          tableName
          (foldl'
             (\acc item ->
                case unTracked item of
                  Constraint{} -> acc
                  Field fieldName fieldType -> Map.insert fieldName (unTracked fieldType) acc
             )
             mempty
             (unTable table)
          )
          (Maybe.isJust $
           Vector.find
             (\case; (unTracked -> Constraint PrimaryKey _) -> True; _ -> False)
             (unTable table)
          )
          constr
          args
      pure . Table $
        Vector.snoc
          (unTable table)
          (Tracked currentMigration $ Constraint constr args)

    runChange ::
      Text ->
      Table ->
      FieldChange ->
      m Table
    runChange tableName table change =
      case change of
        DropField fieldName -> dropField table fieldName
        AddField fieldName fieldType -> addField table fieldName fieldType
        AddConstraint constr args -> addConstraint tableName table constr args

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
