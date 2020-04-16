{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving, UndecidableInstances #-}
{-# language LambdaCase #-}
{-# language OverloadedLists #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Quill.Check.Migration
  ( MigrationError(..)
  , MigrationEnv(..)
  , emptyMigrationEnv
  , checkMigration
  , checkMigrations
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (runStateT)
import Data.Foldable (find, foldl', foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)

import Quill.Check
  ( ColumnInfo(..), ConstraintError
  , DeclEnv(..)
  , TableInfo(..), TableItem(..)
  , TypeError, TypeInfo
  , QueryEnv
  , checkConstraint, mapError
  )
import qualified Quill.Check as Check
import Quill.Syntax (Constraint(..), Type(..))
import Quill.Syntax.Migration (Command(..), FieldChange(..), Migration(..))
import qualified Quill.Syntax.Migration as Migration (Name(..))

data MigrationEnv
  = MigrationEnv
  { _meMigrations :: Map Migration.Name (Migration TypeInfo)
  , _mePath :: Seq Migration.Name
  , _meTables :: Map Text TableInfo
  } deriving (Eq, Show)

emptyMigrationEnv :: MigrationEnv
emptyMigrationEnv =
  MigrationEnv
  { _meMigrations = mempty
  , _mePath = mempty
  , _meTables = mempty
  }

toDeclEnv :: MigrationEnv -> DeclEnv
toDeclEnv env =
  DeclEnv
  { _deLanguage = Nothing
  , _deTypes = mempty
  , _deTables = _meTables env
  , _deGlobalVars = mempty
  , _deGlobalQueries = mempty
  }

toQueryEnv :: MigrationEnv -> QueryEnv Void
toQueryEnv = Check.toQueryEnv . toDeclEnv

data MigrationError typeInfo exprInfo
  = MigrationTableError (Check.TableError typeInfo exprInfo)
  | DuplicateMigration Migration.Name
  | MigrationNotFound Migration.Name
  | TableNotFound Text
  | FieldNotFound Text
  | FieldAlreadyExists Text
  | MigrationConstraintError (ConstraintError TypeInfo)
  | CycleDetected (Seq Migration.Name)
  | MigrationTypeError (TypeError exprInfo)
  deriving (Eq, Show)

dropField :: MonadError (MigrationError typeInfo exprInfo) m => TableInfo -> Text -> m TableInfo
dropField table fieldName =
  let
    cond = (\case; Field n _ -> n == fieldName; _ -> False)
  in
    case Vector.find cond (_tiItems table) of
      Nothing -> throwError $ FieldNotFound fieldName
      Just{} ->
        let
          (droppedCount, columnInfo) = dropFieldColumnInfo $ _tiColumnInfo table
        in
          pure $
          TableInfo
          { _tiNumColumns = _tiNumColumns table - droppedCount
          , _tiColumnInfo = columnInfo
          , _tiReadType = dropFieldType $ _tiReadType table
          , _tiWriteType = dropFieldType $ _tiWriteType table
          , _tiItems = Vector.filter (not . cond) (_tiItems table)
          }
  where
    dropFieldColumnInfo :: Vector (Text, ColumnInfo) -> (Int, Vector (Text, ColumnInfo))
    dropFieldColumnInfo infos =
      let
        cond = (fieldName ==) . fst
      in
        case Vector.find cond infos of
          Nothing -> error $ "impossible: field " <> show fieldName <> " not found"
          Just (_, ci) ->
            ( Vector.length $ Check.flattenColumnInfo ci
            , Vector.filter (not . cond) infos
            )

    dropFieldType :: Type a -> Type a
    dropFieldType ty =
      case ty of
        TRecord tyInfo ts ->
          TRecord tyInfo $ Vector.filter ((fieldName /=) . fst) ts
        _ -> error $ "impossible: dropFieldType must take a TRecord"

addField ::
  MonadError (MigrationError typeInfo exprInfo) m =>
  MigrationEnv ->
  TableInfo ->
  Text ->
  Type typeInfo ->
  m (Type TypeInfo, TableInfo)
addField env table fieldName fieldType =
  let
    cond = (\case; Field n _ -> n == fieldName; _ -> False)
  in
    case Vector.find cond (_tiItems table) of
      Just{} -> throwError $ FieldAlreadyExists fieldName
      Nothing -> do
        fieldType' <-
          mapError MigrationTypeError $ do
            fieldType' <- Check.mkTypeInfo (toQueryEnv env) (Just Check.Column) fieldType
            Check.checkType fieldType'
            pure fieldType'
        (columnInfo, newNumColumns) <- runStateT (Check.mkColumnInfo [(fieldName, fieldType')]) (_tiNumColumns table)
        pure
          ( fieldType'
          , TableInfo
            { _tiNumColumns = newNumColumns
            , _tiColumnInfo =
                _tiColumnInfo table <> columnInfo
            , _tiReadType =
                addFieldType fieldType' (_tiReadType table)
            , _tiWriteType =
                addFieldType fieldType' (_tiReadType table)
            , _tiItems =
                Vector.snoc (_tiItems table) (Field fieldName fieldType')
            }
          )
  where
    addFieldType :: Type TypeInfo -> Type TypeInfo -> Type TypeInfo
    addFieldType new old =
      case old of
        TRecord typeInfo ts -> TRecord typeInfo $ Vector.snoc ts (fieldName, new)
        _ -> error "impossible: addFieldType takes a TRecord"

addConstraint ::
  MonadError (MigrationError typeInfo exprInfo) m =>
  MigrationEnv ->
  Text ->
  TableInfo ->
  Constraint ->
  Vector Text ->
  m TableInfo
addConstraint env tableName table constr args = do
  mapError MigrationConstraintError $
    checkConstraint
      tableName
      (foldl'
         (\acc item ->
           case item of
             Constraint{} -> acc
             Field fieldName fieldType -> Map.insert fieldName fieldType acc
         )
         mempty
         (_tiItems table)
      )
      (Maybe.isJust $
       Vector.find
         (\case; Constraint PrimaryKey _ -> True; _ -> False)
         (_tiItems table)
      )
      constr
      args
  (readType, writeType) <-
    case toUnaryConstraint constr args of
      Nothing -> pure (_tiReadType table, _tiWriteType table)
      Just (field, constr') ->
        mapError MigrationTypeError $
        (,) <$>
        overField
          field
          (fmap snd . Check.mkReadField (toQueryEnv env) field)
          (_tiReadType table) <*>
        overField
          field
          (fmap snd . Check.mkWriteField (toQueryEnv env) (Map.singleton field [constr']) field)
          (_tiWriteType table)
  pure $
    TableInfo
    { _tiNumColumns = _tiNumColumns table
    , _tiColumnInfo = _tiColumnInfo table
    , _tiReadType = readType
    , _tiWriteType = writeType
    , _tiItems =
        Vector.snoc (_tiItems table) (Constraint constr args)
    }
  where
    toUnaryConstraint :: Constraint -> Vector Text -> Maybe (Text, Constraint)
    toUnaryConstraint c as =
      if Vector.length as == 1
      then Just (as Vector.! 0, c)
      else Nothing

    overField ::
      Applicative f =>
      Text ->
      (Type TypeInfo -> f (Type TypeInfo)) ->
      Type TypeInfo ->
      f (Type TypeInfo)
    overField field f ty =
      case ty of
        TRecord tyInfo ts ->
          TRecord tyInfo <$>
          traverse
            (\(n, t) -> if n == field then (,) n <$> f t else pure (n, t))
            ts
        _ -> error "impossible: overField takes a TRecord"

checkCommand ::
  forall typeInfo exprInfo m.
  MonadError (MigrationError typeInfo exprInfo) m =>
  MigrationEnv ->
  Command typeInfo ->
  m (Command TypeInfo, MigrationEnv)
checkCommand env command =
  case command of
    CreateTable tableName fields -> do
      (info, fields') <-
        mapError MigrationTableError $
        Check.checkTable (toDeclEnv env) tableName fields
      pure
        ( CreateTable tableName fields'
        , env { _meTables = Map.insert tableName info $ _meTables env }
        )
    AlterTable tableName changes -> do
      entry <-
        maybe (throwError $ TableNotFound tableName) pure $
        Map.lookup tableName (_meTables env)
      (changes', entry') <-
        foldlM
          (\(cs, e) c -> do
             (c', e') <- runChange tableName e c
             pure (cs . (:) c', e')
          )
          (id, entry)
          changes
      pure
        ( AlterTable tableName $ Vector.fromList (changes' [])
        , env { _meTables = Map.insert tableName entry' $ _meTables env }
        )
  where
    runChange ::
      Text ->
      TableInfo ->
      FieldChange typeInfo ->
      m (FieldChange TypeInfo, TableInfo)
    runChange tableName table change =
      case change of
        DropField fieldName -> do
          info <- dropField table fieldName
          pure (DropField fieldName, info)
        AddField fieldName fieldType -> do
          (fieldType', info) <- addField env table fieldName fieldType
          pure (AddField fieldName fieldType', info)
        AddConstraint constr args -> do
          info <- addConstraint env tableName table constr args
          pure (AddConstraint constr args, info)

checkCommands ::
  MonadError (MigrationError typeInfo exprInfo) m =>
  MigrationEnv ->
  Vector (Command typeInfo) ->
  m (Vector (Command TypeInfo), MigrationEnv)
checkCommands env commands = do
  (commands', env') <-
    foldlM
      (\(cs, e) c -> do
         (c', e') <- checkCommand e c
         pure (cs . (:) c', e')
      )
      (id, env)
      commands
  pure (Vector.fromList $ commands' [], env')

checkMigration ::
  MonadError (MigrationError typeInfo exprInfo) m =>
  (MigrationEnv -> Migration.Name -> m MigrationEnv) ->
  MigrationEnv ->
  Migration typeInfo ->
  m MigrationEnv
checkMigration check env (Migration name m_parent commands) = do
  env' <-
    case m_parent of
      Nothing -> pure env
      Just parent -> check env parent
  (commands', env'') <- checkCommands env' commands
  pure $
    env''
    { _meMigrations =
      Map.insert name (Migration name m_parent commands') $
      _meMigrations env''
    }

checkMigrations ::
  MonadError (MigrationError typeInfo exprInfo) m =>
  Vector (Migration typeInfo) ->
  MigrationEnv ->
  Migration.Name ->
  m MigrationEnv
checkMigrations msInput env root = do
  ms <-
    foldlM
      (\acc m ->
          let
            name = _mName m
          in
            if name `Map.member` acc
            then throwError $ DuplicateMigration name
            else pure $ Map.insert name m acc
      )
      mempty
      msInput
  checkName ms env root
  where
    checkName ms e n =
      case Map.lookup n ms of
        Nothing -> throwError $ MigrationNotFound n
        Just m -> do
          when (Maybe.isJust $ find (n ==) (_mePath e)) . throwError $ CycleDetected (_mePath e)
          let path = _mePath e
          e' <- checkMigration (check ms) (e { _mePath = path Seq.|> n }) m
          pure $ e' { _mePath = path }

    check ms e n =
      if Map.member n (_meMigrations e)
      then pure e
      else checkName ms e n
