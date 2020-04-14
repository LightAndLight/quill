{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Quill.Check
  ( Info(..)
  , TypeInfo(..)
  , anyTypeInfo
  , Origin(..)
  , TableInfo(..)
  , ColumnInfo(..), flattenColumnInfo
  , QueryEnv(..)
  , mkTypeInfo
  , resolveType
  , resolveType'
  , convertExpr
  , TypeError(..)
  , checkExpr
  , inferExpr
  , QueryEntry(..)
  , DeclEnv(..)
  , emptyDeclEnv
  , toQueryEnv
  , ConstraintError(..)
  , checkConstraint
  , DeclError(..)
  , checkDecl
  , checkDecls
    -- * Misc
  , mapError
  )
where

import qualified Bound
import Bound.Scope (Scope(..))
import Bound.Var (unvar)
import Control.Lens.Cons (_Cons)
import Control.Lens.Fold ((^?))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, throwError, runExceptT, withExceptT)
import Control.Monad.State (MonadState, StateT, evalStateT, runStateT, get, gets, modify, put)
import Data.Foldable (foldl', traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Quill.Syntax (Decl, Expr, Language, TableItem, Type)
import qualified Quill.Syntax as Syntax

data TypeInfo
  = TypeInfo
  { _typeInfoOrigin :: Maybe Origin
  } deriving (Eq, Show)

mergeTypeInfo :: TypeInfo -> TypeInfo -> TypeInfo
mergeTypeInfo (TypeInfo m_origin1) (TypeInfo m_origin2) =
  case m_origin1 of
    Nothing -> TypeInfo m_origin2
    Just origin ->
      case m_origin2 of
        Nothing -> TypeInfo m_origin1
        Just origin' ->
          if origin == origin'
          then TypeInfo (Just origin)
          else error "TypeInfo mismatch"

data TypeError t
  = ExpectedRecord (Type TypeInfo)
  | ExpectedMany (Type TypeInfo)
  | ExpectedOptional (Type TypeInfo)
  | ExpectedQuery (Type TypeInfo)
  | MissingField (Type TypeInfo) Text
  | TypeMismatch (Type TypeInfo) (Type TypeInfo)
  | TypeNotInScope Text
  | TableNotInScope Text
  | VariableNotInScope Text
  | Can'tInferExpr (Expr t Text)
  | LanguageMismatch Language (Maybe Language)
  | DuplicateRecordFields
  deriving (Eq, Show)

data ColumnInfo
  = Names (Vector (Text, ColumnInfo))
  | Name Text (Type TypeInfo)
  deriving Show

flattenColumnInfo :: ColumnInfo -> Vector (Text, Type TypeInfo)
flattenColumnInfo f =
  case f of
    Name n ty -> [(n, ty)]
    Names fs' -> fs' >>= flattenColumnInfo . snd

data TableInfo
  = TableInfo
  { _tiNumColumns :: Int
  , _tiColumnInfo :: Vector (Text, ColumnInfo)
  , _tiReadType :: Type TypeInfo
  , _tiWriteType :: Type TypeInfo
  , _tiItems :: Vector (TableItem TypeInfo)
  } deriving Show

data QueryEnv a
  = QueryEnv
  { _qeLanguage :: Maybe Language
  , _qeNames :: a -> Text
  , _qeLocals :: a -> Type TypeInfo
  , _qeGlobalVars :: Map Text (Type TypeInfo)
  , _qeGlobalQueries :: Map Text QueryEntry
  , _qeTypes :: Map Text (Type TypeInfo)
  , _qeTables :: Map Text TableInfo
  }

data Origin
  = Rows Text
  | Row Text
  | Column
  deriving (Eq, Show)

data Info
  = Info
  { _infoType :: Type TypeInfo
  } deriving Show

data DeclEnv
  = DeclEnv
  { _deLanguage :: Maybe Language
  , _deTypes :: Map Text (Type TypeInfo)
  , _deTables :: Map Text TableInfo
  , _deGlobalVars :: Map Text (Type TypeInfo)
  , _deGlobalQueries :: Map Text QueryEntry
  } deriving Show

emptyDeclEnv :: DeclEnv
emptyDeclEnv =
  DeclEnv
  { _deLanguage = Nothing
  , _deTypes = mempty
  , _deTables = mempty
  , _deGlobalVars = mempty
  , _deGlobalQueries = mempty
  }

toQueryEnv :: DeclEnv -> QueryEnv Void
toQueryEnv env =
  QueryEnv
  { _qeLanguage = _deLanguage env
  , _qeNames = absurd
  , _qeLocals = absurd
  , _qeGlobalVars = _deGlobalVars env
  , _qeGlobalQueries = _deGlobalQueries env
  , _qeTypes = _deTypes env
  , _qeTables = _deTables env
  }


resolveType ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Text ->
  m (Type TypeInfo)
resolveType env n =
  maybe
    (throwError $ TypeNotInScope n)
    pure
    (Map.lookup n $ _qeTypes env)

resolveType' ::
  MonadError (TypeError t) m =>
  DeclEnv ->
  Text ->
  m (Type TypeInfo)
resolveType' env n =
  maybe
    (throwError $ TypeNotInScope n)
    pure
    (Map.lookup n $ _deTypes env)

insertAt ::
  Int ->
  Vector (Text, Type TypeInfo) ->
  (Text, Type TypeInfo) ->
  (Bound.Scope () (Expr Info) a, Vector (Text, Type TypeInfo))
insertAt ix fields entry@(field, _) =
  ( Bound.toScope $
    Syntax.IfThenElse
      (Syntax.HasField boundVar field)
      boundVar
      extended
  , prefix <> Vector.cons entry suffix
  )
  where
    boundVar = Syntax.Var $ Bound.B ()
    extended =
      Syntax.Record $
      (keep <$> prefix) <>
      Vector.cons (field, Syntax.None) (keep <$> suffix)
    keep (f, _) =
      (f, Syntax.Project boundVar f)
    (prefix, suffix) = Vector.splitAt ix fields

convertFields ::
  forall t m a.
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Vector (Text, Type TypeInfo) ->
  Vector (Text, Type TypeInfo) ->
  m (Maybe (Bound.Scope () (Expr Info) a, Vector (Text, Type TypeInfo)))
convertFields env e a = do
  (res, _) <- go 0 a e a
  pure res
  where
    go ::
      Int ->
      Vector (Text, Type TypeInfo) ->
      Vector (Text, Type TypeInfo) ->
      Vector (Text, Type TypeInfo) ->
      m
        ( Maybe (Bound.Scope () (Expr Info) a, Vector (Text, Type TypeInfo))
        , Vector (Text, Type TypeInfo)
        )
    go !ix full expected actual =
      case expected ^? _Cons of
        Nothing ->
          case actual ^? _Cons of
            Nothing -> pure (Just (identity, []), full)
            Just{} -> pure (Nothing, full)
        Just ((field, ty), fields) ->
          case actual ^? _Cons of
            Just ((field', ty'), fields') | field == field' -> do
              (m_g, full') <- go (ix+1) full fields fields'
              case m_g of
                Nothing -> pure (Nothing, full')
                Just (g, fields'') -> do
                  (f, ty'') <- convertExpr env ty ty'
                  let
                    f' =
                      Bound.toScope $
                      Syntax.Update
                        field
                        ("__temp", Syntax.Scope2 $ Bound.F <$> f)
                        (Syntax.Var $ Bound.B ())
                  pure (Just (Syntax.compose g f', Vector.cons (field, ty'') fields''), full')
            _ ->
              case ty of
                Syntax.TOptional{} -> do
                  let (f, full') = insertAt ix full (field, ty)
                  (m_g, full'') <- go (ix+1) full' fields actual
                  case m_g of
                    Nothing -> pure (Nothing, full'')
                    Just (g, fields') -> do
                      pure (Just (Syntax.compose g f, Vector.cons (field, ty) fields'), full'')
                _ -> pure (Nothing, full)

identity :: Monad f => Bound.Scope () f b
identity = Bound.toScope (pure $ Bound.B ())

mapOptional ::
  Scope () (Expr Info) a ->
  Scope () (Expr Info) a
mapOptional f =
  Bound.toScope $
    Syntax.FoldOptional
      Syntax.None
      ( "__temp"
      , Syntax.Scope2 $
        Bound.F <$>
        Syntax.compose
          (Bound.toScope $ Syntax.Some $ Syntax.Var $ Bound.B ())
          f
      )
      (Syntax.Var $ Bound.B ())

mapMany :: Scope () (Expr Info) a -> Scope () (Expr Info) a
mapMany f =
  Bound.toScope $
    Syntax.For
      "__temp"
      (Syntax.Var $ Bound.B ())
      Nothing
      (Syntax.toScope2 $ unvar Bound.B (Bound.F . Bound.F) <$> Bound.fromScope f)

convertExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Type TypeInfo ->
  Type TypeInfo ->
  m (Bound.Scope () (Expr Info) a, Type TypeInfo)
convertExpr env expected actual =
  case expected of
    Syntax.TName _ n -> do
      expected' <- resolveType env n
      convertExpr env expected' actual
    _ | Syntax.TName _ n <- actual -> do
      actual' <- resolveType env n
      convertExpr env expected actual'
    Syntax.TRecord tyInfo fields ->
      case actual of
        Syntax.TRecord tyInfo' fields' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          m_fields'' <- convertFields env fields fields'
          case m_fields'' of
            Nothing -> throwError $ TypeMismatch expected actual
            Just (expr', fields'') -> pure (expr', Syntax.TRecord tyInfo'' fields'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TInt tyInfo ->
      case actual of
        Syntax.TInt tyInfo' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          pure (identity, Syntax.TInt tyInfo'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TUnit tyInfo ->
      case actual of
        Syntax.TUnit tyInfo' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          pure (identity, Syntax.TUnit tyInfo'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TBool tyInfo ->
      case actual of
        Syntax.TBool tyInfo' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          pure (identity, Syntax.TBool tyInfo'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TQuery tyInfo a -> do
      case actual of
        Syntax.TQuery tyInfo' a' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          (f, a'') <- convertExpr env a a'
          pure (mapQuery f, Syntax.TQuery tyInfo'' a'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TMany tyInfo a -> do
      case actual of
        Syntax.TMany tyInfo' a' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          (f, a'') <- convertExpr env a a'
          pure (mapMany f, Syntax.TMany tyInfo'' a'')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TOptional tyInfo a -> do
      case actual of
        Syntax.TOptional tyInfo' a' -> do
          let !tyInfo'' = mergeTypeInfo tyInfo tyInfo'
          (f, a'') <- convertExpr env a a'
          pure (mapOptional f, Syntax.TOptional tyInfo'' a'')
        _ -> throwError $ TypeMismatch expected actual

language :: MonadError (TypeError t) m => QueryEnv a -> Language -> m ()
language env l =
  unless (Just l == _qeLanguage env) . throwError $
  LanguageMismatch l (_qeLanguage env)

checkType ::
  MonadError (TypeError t) m =>
  Type TypeInfo ->
  m ()
checkType ty =
  case ty of
    Syntax.TRecord _ fields ->
      let
        names = foldr (Set.insert . fst) mempty fields
      in
        if Set.size names == Vector.length fields
        then traverse_ (checkType . snd) fields
        else throwError DuplicateRecordFields
    Syntax.TUnit _ -> pure ()
    Syntax.TBool _ -> pure ()
    Syntax.TMany _ a -> checkType a
    Syntax.TQuery _ a -> checkType a
    Syntax.TOptional _ a -> checkType a
    Syntax.TName _ _ -> pure ()
    Syntax.TInt _ -> pure ()

mapQuery :: Bound.Scope () (Expr Info) a -> Bound.Scope () (Expr Info) a
mapQuery f =
  Bound.toScope . Syntax.Bind (Syntax.Var $ Bound.B ()) "__temp" .
  Syntax.toScope2 . Syntax.Return $
  unvar Bound.B (Bound.F . Bound.F) <$> Bound.fromScope f

checkExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Expr t a ->
  Type TypeInfo ->
  m (Expr Info a, Type TypeInfo)
checkExpr env expr ty = do
  case expr of
    Syntax.Many values ->
      case ty of
        Syntax.TMany tyInfo ty' -> do
          (exprs, tys) <- Vector.unzip <$> traverse (\e -> checkExpr env e ty') values
          let
            inner = foldl' (\acc t -> mergeTypeInfo acc (Syntax.getTypeAnn t)) anyTypeInfo tys
            !tyInfo' =
              mergeTypeInfo tyInfo $
              case _typeInfoOrigin inner of
                Just (Row table) -> inner { _typeInfoOrigin = Just $ Rows table }
                Nothing -> inner { _typeInfoOrigin = Nothing }
                origin -> error $ "wierd origin for Many's argument: " <> show origin
          pure
            ( Syntax.Many exprs
            , Syntax.TMany tyInfo' $ Maybe.fromMaybe ty' (tys Vector.!? 0)
            )
        _ -> throwError $ ExpectedMany ty
    Syntax.None ->
      case ty of
        Syntax.TOptional{} -> pure (Syntax.None, ty)
        _ -> throwError $ ExpectedOptional ty
    Syntax.Record fields ->
      case ty of
        Syntax.TRecord tyInfo fieldTys -> do
          (expr', ty') <- inferRecord env (Just tyInfo, foldr (uncurry Map.insert) mempty fieldTys) fields
          (f, ty'') <- convertExpr env ty ty'
          pure (Bound.instantiate1 expr' f, ty'')
        _ -> throwError $ ExpectedRecord ty
    _ -> do
      (expr', ty') <- inferExpr env expr
      (f, ty'') <- convertExpr env ty ty'
      pure (Bound.instantiate1 expr' f, ty'')

inferRecord ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  (Maybe TypeInfo, Map Text (Type TypeInfo)) ->
  Vector (Text, Expr t a) ->
  m (Expr Info a, Type TypeInfo)
inferRecord env (m_tyInfo, hints) fields =
  let
    names = foldr (Set.insert . fst) mempty fields
  in
    if Set.size names == Vector.length fields
    then do
      results <-
        traverse
          (\(f, v) ->
             case Map.lookup f hints of
               Just vTy -> do
                 (v', vTy') <- checkExpr env v vTy
                 pure (f, (v', vTy'))
               Nothing -> (,) f <$> inferExpr env v
          )
          fields
      pure
        ( Syntax.Record $ (\(n, (val, _)) -> (n, val)) <$> results
        , Syntax.TRecord (Maybe.fromMaybe anyTypeInfo m_tyInfo) $ (\(n, (_, ty)) -> (n, ty)) <$> results
        )
    else throwError DuplicateRecordFields

anyTypeInfo :: TypeInfo
anyTypeInfo = TypeInfo { _typeInfoOrigin = Nothing }

inferExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Expr t a ->
  m (Expr Info a, Type TypeInfo)
inferExpr env expr =
  case expr of
    Syntax.Info _ a -> inferExpr env a
    Syntax.SelectFrom table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      pure
        ( Syntax.SelectFrom table
        , Syntax.TQuery anyTypeInfo $
          Syntax.TMany
            (anyTypeInfo { _typeInfoOrigin = Just $ Rows table })
            (_tiReadType info)
        )
    Syntax.InsertInto value table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      (value', _) <- checkExpr env value (_tiWriteType info)
      pure (Syntax.InsertInto value' table, Syntax.TQuery anyTypeInfo $ Syntax.TUnit anyTypeInfo)
    Syntax.InsertIntoReturning value table -> do
      language env Syntax.Postgresql
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      (value', valueTy) <- checkExpr env value (_tiWriteType info)
      let
        !_ =
          mergeTypeInfo
            (Syntax.getTypeAnn valueTy)
            (anyTypeInfo { _typeInfoOrigin = Just $ Row table })
      pure
        ( Syntax.InsertIntoReturning value' table
        , Syntax.TQuery anyTypeInfo $
          Syntax.TMany
            (anyTypeInfo { _typeInfoOrigin = Just $ Rows table })
            (_tiReadType info)
        )
    Syntax.Bind value n rest -> do
      (value', valueTy) <- inferExpr env value
      case valueTy of
        Syntax.TQuery tyInfo ty -> do
          (rest', restTy) <-
            inferExpr
              (env
               { _qeNames = unvar (\() -> n) (_qeNames env)
               , _qeLocals = unvar (\() -> ty) (_qeLocals env)
               }
              )
              (Syntax.fromScope2 rest)
          case restTy of
            Syntax.TQuery tyInfo' ty' -> do
              let tyInfo'' = mergeTypeInfo tyInfo tyInfo'
              pure (Syntax.Bind value' n $ Syntax.toScope2 rest', Syntax.TQuery tyInfo'' ty')
            _ -> throwError $ ExpectedQuery restTy
        _ -> throwError $ ExpectedQuery valueTy
    Syntax.Return value -> do
      (value', valueTy) <- inferExpr env value
      pure (Syntax.Return value', Syntax.TQuery anyTypeInfo valueTy)
    Syntax.Some a -> do
      (a', aTy) <- inferExpr env a
      pure (Syntax.Some a', Syntax.TOptional anyTypeInfo aTy)
    Syntax.None -> throwError $ Can'tInferExpr (_qeNames env <$> expr)
    Syntax.FoldOptional z (n, f) value -> do
      (value', valueTy) <- inferExpr env value
      case valueTy of
        Syntax.TOptional _ a -> do
          (z', zTy) <- inferExpr env z
          let
            env' =
              env
              { _qeNames =
                  unvar (\() -> n) (_qeNames env)
              , _qeLocals =
                  unvar
                    (\() -> a)
                    (_qeLocals env)
              }
          (f', _) <- checkExpr env' (Syntax.fromScope2 f) zTy
          pure (Syntax.FoldOptional z' (n, Syntax.toScope2 f') value', zTy)
        _ -> throwError $ ExpectedOptional valueTy
    Syntax.Many values
      | Vector.length values > 0 -> do
          (head', headTy) <- inferExpr env (Vector.head values)
          (tail', tailTys) <-
            Vector.unzip <$>
            traverse
              (\value -> checkExpr env value headTy)
              (Vector.tail values)
          let
            tyInfo =
              foldl'
                (\acc t -> mergeTypeInfo acc (Syntax.getTypeAnn t))
                (Syntax.getTypeAnn headTy)
                tailTys
            tyInfo' =
              tyInfo
              { _typeInfoOrigin = do
                  origin <- _typeInfoOrigin tyInfo
                  case origin of
                    Row table -> pure $ Rows table
                    _ -> pure origin
              }
          pure
            ( Syntax.Many $ Vector.cons head' tail'
            , Syntax.TMany tyInfo' headTy
            )
      | otherwise ->
          throwError $ Can'tInferExpr (_qeNames env <$> expr)
    Syntax.Int n -> pure (Syntax.Int n, Syntax.TInt anyTypeInfo)
    Syntax.Unit -> pure (Syntax.Unit, Syntax.TUnit anyTypeInfo)
    Syntax.Bool b -> pure (Syntax.Bool b, Syntax.TBool anyTypeInfo)
    Syntax.IfThenElse a b c -> do
      (a', _) <- checkExpr env a $ Syntax.TBool anyTypeInfo
      (b', bTy) <- inferExpr env b
      (c', _) <- checkExpr env c bTy
      pure (Syntax.IfThenElse a' b' c', bTy)
    Syntax.For n value m_cond yield -> do
      (value', valTy) <- inferExpr env value
      case valTy of
        Syntax.TMany tyInfo itemTy -> do
          let
            env' =
              env
              { _qeNames =
                  unvar (\() -> n) (_qeNames env)
              , _qeLocals =
                  unvar
                    (\() -> itemTy)
                    (_qeLocals env)
              }
          m_cond' <-
            case m_cond of
              Nothing -> pure Nothing
              Just cond ->
                Just . Syntax.toScope2 . fst <$>
                checkExpr
                  env'
                  (Syntax.fromScope2 cond)
                  (Syntax.TBool anyTypeInfo)
          (yield', yieldTy) <-
            inferExpr
              env'
              (Syntax.fromScope2 yield)
          pure (Syntax.For n value' m_cond' $ Syntax.toScope2 yield', Syntax.TMany tyInfo yieldTy)
        _ -> throwError $ ExpectedMany valTy
    Syntax.Name n ->
      (,) (Syntax.Name n) <$>
      maybe
        (throwError $ VariableNotInScope n)
        pure
        (Map.lookup n $ _qeGlobalVars env)
    Syntax.Extend field value rest -> do
      (rest', restTy) <- inferExpr env rest
      case restTy of
        Syntax.TRecord tyInfo fields -> do
          case Vector.find ((field ==) . fst) fields of
            Nothing -> do
              (value', valueTy) <- inferExpr env value
              pure
                ( Syntax.Extend field value' rest'
                , Syntax.TRecord tyInfo $ Vector.cons (field, valueTy) fields
                )
            Just{} -> throwError $ DuplicateRecordFields
        _ -> throwError $ ExpectedRecord restTy
    Syntax.Update field (n, fun) record -> do
      (record', recordTy) <- inferExpr env record
      case recordTy of
        Syntax.TRecord tyInfo fields ->
          case Vector.find ((field ==) . fst) fields of
            Nothing -> throwError $ MissingField recordTy field
            Just (_, fieldTy) -> do
              let
                env' =
                  env
                  { _qeNames =
                      unvar (\() -> n) (_qeNames env)
                  , _qeLocals =
                      unvar
                        (\() -> fieldTy)
                        (_qeLocals env)
                  }
              (fun', fieldTy') <- inferExpr env' (Syntax.fromScope2 fun)
              pure
                ( Syntax.Update field (n, Syntax.toScope2 fun') record'
                , Syntax.TRecord tyInfo $
                  (\(f, t) -> if f == field then (f, fieldTy') else (f, t)) <$>
                  fields
                )
        _ -> throwError $ ExpectedRecord recordTy
    Syntax.Record fields -> inferRecord env (Nothing, mempty) fields
    Syntax.Project val field -> do
      (val', valTy) <- inferExpr env val
      case valTy of
        Syntax.TRecord _ fields ->
          case Vector.find ((field ==) . fst) fields of
            Nothing -> throwError $ MissingField valTy field
            Just (_, fieldTy) -> do
              let info = Info { _infoType = fieldTy }
              pure (Syntax.Info info $ Syntax.Project val' field, fieldTy)
        _ -> throwError $ ExpectedRecord valTy
    Syntax.HasField record field -> do
      (record', recordTy) <- inferExpr env record
      case recordTy of
        Syntax.TRecord{} -> pure (Syntax.HasField record' field, Syntax.TBool anyTypeInfo)
        _ -> throwError $ ExpectedRecord recordTy

    Syntax.Var n -> do
      let
        ty = _qeLocals env n
        info = Info { _infoType = ty }
      pure (Syntax.Info info $ Syntax.Var n, ty)

    Syntax.AND a b -> do
      (a', _) <- checkExpr env a $ Syntax.TBool anyTypeInfo
      (b', _) <- checkExpr env b $ Syntax.TBool anyTypeInfo
      pure (Syntax.AND a' b', Syntax.TBool anyTypeInfo)
    Syntax.OR a b -> do
      (a', _) <- checkExpr env a $ Syntax.TBool anyTypeInfo
      (b', _) <- checkExpr env b $ Syntax.TBool anyTypeInfo
      pure (Syntax.OR a' b', Syntax.TBool anyTypeInfo)
    Syntax.EQ a b -> do
      (a', aTy) <- inferExpr env a
      (b', _) <- checkExpr env b aTy
      pure (Syntax.EQ a' b', Syntax.TBool anyTypeInfo)
    Syntax.NOT a -> do
      (a', _) <- checkExpr env a $ Syntax.TBool anyTypeInfo
      pure (Syntax.NOT a', Syntax.TBool anyTypeInfo)

data QueryEntry
  = QueryEntry
  { _qeArgTys :: Vector (Type TypeInfo)
  , _qeRetTy :: Type TypeInfo
  , _qeBody :: Scope Int (Expr Info) Void
  } deriving Show

data ConstraintError t'
  = ConstraintArgsMismatch Int Int
  | FieldNotInScope Text
  | UnknownConstraint Text
  | NotEnumerable (Type t')
  | MultiplePrimaryKeys Text
  deriving (Eq, Show)

data DeclError t t'
  = ConstraintError (ConstraintError t')
  | FieldAlreadyDefined Text
  | TypeAlreadyDefined Text
  | TableAlreadyDefined Text
  | VariableAlreadyDefined Text
  | DuplicateArgument Text
  | TypeError (TypeError t)
  deriving Show

isEnumerable :: Type () -> Bool
isEnumerable ty = ty `Set.member` tys
  where
    tys = [Syntax.TInt ()]

data TableItemState t
  = TableItemState
  { _hasPrimaryKey :: Bool
  , _fieldsSeen :: Map Text (Type t)
  }

checkConstraint ::
  MonadError (ConstraintError t') m =>
  Text -> -- table name
  Map Text (Type t') -> -- field types
  Bool -> -- does the table have a primary key?
  Syntax.Constraint ->
  Vector Text ->
  m ()
checkConstraint tableName fieldTypes hasPrimaryKey constr args =
  case constr of
    Syntax.AutoIncrement -> do
      let argsLength = Vector.length args
      unless (argsLength == 1) . throwError $ ConstraintArgsMismatch 1 argsLength
      let arg = args Vector.! 0
      argTy <- maybe (throwError $ FieldNotInScope arg) pure $ Map.lookup arg fieldTypes
      unless (isEnumerable $ () <$ argTy) . throwError $ NotEnumerable argTy
    Syntax.PrimaryKey -> do
      when hasPrimaryKey . throwError $ MultiplePrimaryKeys tableName
      traverse_
        (\arg -> maybe (throwError $ FieldNotInScope arg) pure $ Map.lookup arg fieldTypes)
        args
    Syntax.Other name -> throwError $ UnknownConstraint name

checkTableItem ::
  ( MonadError (DeclError t t') m
  , MonadState (TableItemState t') m
  ) =>
  DeclEnv ->
  Text ->
  TableItem t' ->
  m (TableItem TypeInfo)
checkTableItem env tableName item =
  case item of
    Syntax.Field name ty -> do
      m_ty <- gets (Map.lookup name . _fieldsSeen)
      case m_ty of
        Nothing -> do
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = absurd
              , _qeLocals = absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }
          ty' <- mapError TypeError $ mkTypeInfo queryEnv (Just Column) ty
          mapError TypeError $ checkType ty'
          modify $ \s -> s { _fieldsSeen = Map.insert name ty (_fieldsSeen s) }
          pure $ Syntax.Field name ty'
        Just{} -> throwError $ FieldAlreadyDefined name
    Syntax.Constraint constr args -> do
      fieldsSeen <- gets _fieldsSeen
      hasPrimaryKey <- gets _hasPrimaryKey
      mapError ConstraintError $ checkConstraint tableName fieldsSeen hasPrimaryKey constr args
      case constr of
        Syntax.PrimaryKey -> modify $ \s -> s { _hasPrimaryKey = True }
        _ -> pure ()
      pure $ Syntax.Constraint constr args

checkDeclArg ::
  ( MonadError (DeclError t t') m
  , MonadState (Set Text) m
  ) =>
  DeclEnv ->
  (Text, Type a) ->
  m (Text, Type TypeInfo)
checkDeclArg env (name, ty) = do
  seen <- gets $ Set.member name
  when seen . throwError $ DuplicateArgument name
  let
    queryEnv =
      QueryEnv
      { _qeLanguage = _deLanguage env
      , _qeNames = absurd
      , _qeLocals = absurd
      , _qeGlobalVars = _deGlobalVars env
      , _qeGlobalQueries = _deGlobalQueries env
      , _qeTypes = _deTypes env
      , _qeTables = _deTables env
      }
  ty' <- mapError TypeError $ mkTypeInfo queryEnv Nothing ty
  (name, ty') <$ mapError TypeError (checkType ty')

mapError :: MonadError b m => (a -> b) -> ExceptT a m x -> m x
mapError f = (either throwError pure =<<) . runExceptT . withExceptT f

checkDecl ::
  MonadError (DeclError t t') m =>
  DeclEnv ->
  Decl t t' ->
  m (Decl Info TypeInfo)
checkDecl env decl =
  case decl of
    Syntax.Table tableName items -> do
      case Map.lookup tableName (_deTables env) of
        Nothing -> pure ()
        Just{} -> throwError $ TableAlreadyDefined tableName
      items' <-
        evalStateT
          (traverse (checkTableItem env tableName) items)
          (TableItemState False mempty)
      pure $ Syntax.Table tableName items'
    Syntax.Type name ty ->
      case Map.lookup name (_deTypes env) of
        Nothing -> do
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = absurd
              , _qeLocals = absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }
          ty' <- mapError TypeError $ mkTypeInfo queryEnv Nothing ty
          Syntax.Type name ty' <$ mapError TypeError (checkType ty')
        Just{} -> throwError $ TypeAlreadyDefined name
    Syntax.Query name args retTy body ->
      case Map.member name (_deGlobalVars env) || Map.member name (_deGlobalQueries env) of
        True -> throwError $ VariableAlreadyDefined name
        False -> do
          args' <- evalStateT (traverse (checkDeclArg env) args) mempty
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = unvar (fst . (args' Vector.!)) absurd
              , _qeLocals = unvar (snd . (args' Vector.!)) absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }
          retTy' <- mapError TypeError $ mkTypeInfo queryEnv Nothing retTy
          case retTy' of
            Syntax.TQuery{} -> pure ()
            _ -> throwError . TypeError $ ExpectedQuery retTy'
          (body', retTy'') <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy'
          pure $ Syntax.Query name args' retTy'' (Bound.toScope body')
    Syntax.Function name args retTy body ->
      case Map.member name (_deGlobalVars env) || Map.member name (_deGlobalQueries env) of
        True -> throwError $ VariableAlreadyDefined name
        False -> do
          args' <- evalStateT (traverse (checkDeclArg env) args) mempty
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = unvar (fst . (args' Vector.!)) absurd
              , _qeLocals = unvar (snd . (args' Vector.!)) absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }

          retTy' <- mapError TypeError $ mkTypeInfo queryEnv Nothing retTy
          (body', retTy'') <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy'
          pure $ Syntax.Function name args' retTy'' (Bound.toScope body')

mkTypeInfo ::
  MonadError (TypeError t) m =>
  QueryEnv x ->
  Maybe Origin ->
  Type a ->
  m (Type TypeInfo)
mkTypeInfo env m_origin t =
  case t of
    Syntax.TRecord _ fields ->
      Syntax.TRecord (TypeInfo { _typeInfoOrigin = m_origin }) <$>
        ((traverse.traverse)
           (mkTypeInfo env $ do
              origin <- m_origin
              case origin of
                Row _ -> pure Column
                _ -> Nothing
           )
           fields
        )
    Syntax.TUnit _ ->
      pure $ Syntax.TUnit (TypeInfo { _typeInfoOrigin = m_origin })
    Syntax.TBool _ ->
      pure $ Syntax.TBool (TypeInfo { _typeInfoOrigin = m_origin })
    Syntax.TInt _ ->
      pure $ Syntax.TInt (TypeInfo { _typeInfoOrigin = m_origin })
    Syntax.TMany _ a ->
      Syntax.TMany (TypeInfo { _typeInfoOrigin = m_origin }) <$>
      mkTypeInfo env (do
        origin <- m_origin
        case origin of
          Rows table -> pure $ Row table
          _ -> Nothing
      )
      a
    Syntax.TQuery _ a ->
      Syntax.TQuery (TypeInfo { _typeInfoOrigin = m_origin }) <$>
      mkTypeInfo env Nothing a
    Syntax.TOptional _ a ->
      Syntax.TOptional (TypeInfo { _typeInfoOrigin = m_origin }) <$>
      mkTypeInfo env Nothing a
    Syntax.TName _ n -> mkTypeInfo env m_origin =<< resolveType env n

mkTableInfo ::
  forall t m.
  MonadError (TypeError t) m =>
  QueryEnv Void ->
  Text ->
  Vector (TableItem TypeInfo) ->
  m TableInfo
mkTableInfo env table items = do
  readFields <- mkReadFields
  writeFields <- mkWriteFields readFields
  (columnInfo, numColumns) <- runStateT (mkColumnInfo readFields) 0
  let info = TypeInfo { _typeInfoOrigin = Just $ Row table }
  pure $
    TableInfo
    { _tiNumColumns = numColumns
    , _tiColumnInfo = columnInfo
    , _tiReadType = Syntax.TRecord info readFields
    , _tiWriteType = Syntax.TRecord info writeFields
    , _tiItems = items
    }
  where
    mkColumnInfo ::
      Vector (Text, Type TypeInfo) ->
      StateT Int m (Vector (Text, ColumnInfo))
    mkColumnInfo = traverse (\(f, v) -> (,) f <$> go (Builder.fromText f) v)
      where
        go ::
          Builder.Builder ->
          Type TypeInfo ->
          StateT Int m ColumnInfo
        go n ty =
          case ty of
            Syntax.TRecord _ ns' ->
              Names <$> traverse (\(f, v) -> (,) f <$> go (n <> "_" <> Builder.fromText f) v) ns'
            _ -> do
              modify (+1)
              pure $ Name (Lazy.toStrict $ Builder.toLazyText n) ty

    unaryConstraints =
      Vector.foldr
        (\case
           Syntax.Constraint constr args
             | Vector.length args == 1 ->
                 Map.insertWith
                   (<>)
                   (args Vector.! 0)
                   (Set.singleton constr)
             | otherwise -> id
           _ -> id
        )
        mempty
        items
    mkReadFields =
      Vector.mapMaybe id <$>
      traverse
        (\case
          Syntax.Field n ty -> Just . (,) n <$> mkTypeInfo env (Just Column) ty
          _ -> pure Nothing
        )
        items
    mkWriteFields =
      traverse
        (\(n, ty) ->
           case Map.lookup n unaryConstraints of
             Nothing -> (,) n <$> mkTypeInfo env (Just Column) ty
             Just cs ->
               if Syntax.AutoIncrement `Set.member` cs
               then
                 (,) n . Syntax.TOptional ((Syntax.getTypeAnn ty) { _typeInfoOrigin = Just Column }) <$>
                 mkTypeInfo env Nothing ty
               else (,) n <$> mkTypeInfo env (Just Column) ty
        )

checkDecls ::
  forall t t' m.
  MonadError (DeclError t t') m =>
  DeclEnv ->
  Vector (Decl t t') ->
  m (Vector (Decl Info TypeInfo), DeclEnv)
checkDecls e decls = runStateT (traverse go decls) e
  where
    go ::
      forall m'.
      (MonadState DeclEnv m', MonadError (DeclError t t') m') =>
      Decl t t' ->
      m' (Decl Info TypeInfo)
    go decl = do
      env <- get
      decl' <- checkDecl env decl
      case decl' of
        Syntax.Type name val ->
          put $ env { _deTypes = Map.insert name val (_deTypes env) }
        Syntax.Table name items -> do
          let
            env' =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = absurd
              , _qeLocals = absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }
          items' <- mapError TypeError $ mkTableInfo env' name items
          put $ env { _deTables = Map.insert name items' (_deTables env) }
        Syntax.Query name args retTy body ->
          put $
          env
          { _deGlobalQueries =
            Map.insert name (QueryEntry (snd <$> args) retTy body) (_deGlobalQueries env)
          }
        Syntax.Function name args retTy _ -> error "todo: add function to scope" name args retTy
      pure decl'
