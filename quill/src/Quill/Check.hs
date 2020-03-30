{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Quill.Check
  ( Info(..)
  , TypeInfo(..)
  , Origin(..)
  , TableInfo(..)
  , QueryEnv(..)
  , mkTypeInfo
  , resolveType
  , convertExpr
  , TypeError(..)
  , checkExpr
  , inferExpr
  , DeclEnv(..)
  , DeclError(..)
  , checkDecl
  , checkDecls
  )
where

import qualified Bound
import Bound.Scope (Scope(..))
import Bound.Var (unvar)
import Control.Lens.Cons (_Cons)
import Control.Lens.Fold ((^?))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError, throwError, runExceptT, withExceptT)
import Control.Monad.State (MonadState, evalStateT, gets, modify)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Quill.Syntax (Decl, Expr, Language, TableItem, Type)
import qualified Quill.Syntax as Syntax

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
  | LanguageMismatch Language Language
  | DuplicateRecordFields
  deriving (Eq, Show)

data TableInfo
  = TableInfo
  { _tiReadType :: Type TypeInfo
  , _tiWriteType :: Type TypeInfo
  }

data QueryEnv a
  = QueryEnv
  { _qeLanguage :: Language
  , _qeNames :: a -> Text
  , _qeLocals :: a -> Type TypeInfo
  , _qeGlobalVars :: Map Text (Type TypeInfo)
  , _qeGlobalQueries :: Map Text (Type TypeInfo)
  , _qeTypes :: Map Text (Type TypeInfo)
  , _qeTables :: Map Text TableInfo
  }

data Origin
  = Rows
  | Row
  | Column
  deriving (Eq, Show)

data TypeInfo
  = TypeInfo
  { _typeInfoOrigin :: Maybe Origin
  } deriving (Eq, Show)

data Info
  = Info
  { _infoType :: Type TypeInfo
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

compose :: Monad f => Scope () f a -> Scope () f a -> Scope () f a
compose f (Bound.fromScope -> g) = Bound.toScope $ unvar (\() -> g) (pure . Bound.F) =<< Bound.fromScope f

insertAt ::
  Int ->
  Vector (Text, Type TypeInfo) ->
  (Text, Type TypeInfo) ->
  (Bound.Scope () (Expr Info) a, Vector (Text, Type TypeInfo))
insertAt ix fields entry@(field, _) =
  ( Bound.toScope $
    Syntax.IfThenElse
      (Syntax.HasField (Syntax.Var $ Bound.B ()) field)
      (Syntax.Var $ Bound.B ())
      extended
  , prefix <> Vector.cons entry suffix
  )
  where
    extended =
      Syntax.Record $
      (keep <$> prefix) <>
      Vector.cons (field, Syntax.None) (keep <$> suffix)
    keep (f, v) =
      let info = Info { _infoType = v }
      in
        (f, Syntax.Project info (Syntax.Var (Bound.B ())) f)
    (prefix, suffix) = Vector.splitAt ix fields

convertFields ::
  forall t m a.
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Vector (Text, Type TypeInfo) ->
  Vector (Text, Type TypeInfo) ->
  m (Maybe (Bound.Scope () (Expr Info) a))
convertFields env e a = do
  (res, _) <- go 0 a e a
  pure res
  where
    go ::
      Int ->
      Vector (Text, Type TypeInfo) ->
      Vector (Text, Type TypeInfo) ->
      Vector (Text, Type TypeInfo) ->
      m (Maybe (Bound.Scope () (Expr Info) a), Vector (Text, Type TypeInfo))
    go !ix full expected actual =
      case expected ^? _Cons of
        Nothing ->
          case actual ^? _Cons of
            Nothing -> pure (Just  identity, full)
            Just{} -> pure (Nothing, full)
        Just ((field, ty), fields) ->
          case actual ^? _Cons of
            Just ((field', ty'), fields')
              | field == field' -> do
                  (m_g, full') <- go (ix+1) full fields fields'
                  case m_g of
                    Nothing -> pure (Nothing, full')
                    Just g -> do
                      f <- convertExpr env ty ty'
                      let f' = Bound.toScope $ Syntax.Update field ("__temp", Syntax.Scope2 $ Bound.F <$> f) (Syntax.Var $ Bound.B ())
                      pure (Just $ compose g f', full')
            _ ->
              case ty of
                Syntax.TOptional{} -> do
                  let (f, full') = insertAt ix full (field, ty)
                  (m_g, full'') <- go (ix+1) full' fields actual
                  case m_g of
                    Nothing -> pure (Nothing, full'')
                    Just g -> do
                      pure (Just $ compose g f, full'')
                _ -> pure (Nothing, full)

identity :: Monad f => Bound.Scope () f b
identity = Bound.toScope (pure $ Bound.B ())

mapOptional :: Scope () (Expr t) a -> Scope () (Expr t) a
mapOptional f =
  Bound.toScope $
    Syntax.FoldOptional
      Syntax.None
      ( "__temp"
      , Syntax.Scope2 $
        Bound.F <$>
        compose
          (Bound.toScope $ Syntax.Some $ Syntax.Var $ Bound.B ())
          f
      )
      (Syntax.Var $ Bound.B ())

mapMany :: Scope () (Expr t) a -> Scope () (Expr t) a
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
  m (Bound.Scope () (Expr Info) a)
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
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          m_fields'' <- convertFields env fields fields'
          case m_fields'' of
            Nothing -> throwError $ TypeMismatch expected actual
            Just expr' -> pure expr'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TInt tyInfo ->
      case actual of
        Syntax.TInt tyInfo' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TUnit tyInfo ->
      case actual of
        Syntax.TUnit tyInfo' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TBool tyInfo ->
      case actual of
        Syntax.TBool tyInfo' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TQuery tyInfo a -> do
      case actual of
        Syntax.TQuery tyInfo' a' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          mapQuery <$> convertExpr env a a'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TMany tyInfo a -> do
      case actual of
        Syntax.TMany tyInfo' a' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          mapMany <$> convertExpr env a a'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TOptional tyInfo a -> do
      case actual of
        Syntax.TOptional tyInfo' a' -> do
          unless (tyInfo == tyInfo') $ error "type info mismatch"
          mapOptional <$> convertExpr env a a'
        _ -> throwError $ TypeMismatch expected actual

language :: MonadError (TypeError t) m => QueryEnv a -> Language -> m ()
language env l =
  unless (l == _qeLanguage env) . throwError $
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

mapQuery :: Bound.Scope () (Expr t) a -> Bound.Scope () (Expr t) a
mapQuery f =
  Bound.toScope . Syntax.Bind (Syntax.Var $ Bound.B ()) "__temp" .
  Syntax.toScope2 . Syntax.Return $
  unvar Bound.B (Bound.F . Bound.F) <$> Bound.fromScope f

checkExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Expr t a ->
  Type TypeInfo ->
  m (Expr Info a)
checkExpr env expr ty = do
  case expr of
    Syntax.Many values ->
      case ty of
        Syntax.TMany _ ty' ->
          Syntax.Many <$> traverse (\e -> checkExpr env e ty') values
        _ -> throwError $ ExpectedMany ty
    Syntax.None ->
      case ty of
        Syntax.TOptional{} -> pure Syntax.None
        _ -> throwError $ ExpectedOptional ty
    Syntax.Record fields ->
      case ty of
        Syntax.TRecord tyInfo fieldTys -> do
          (expr', ty') <- inferRecord env (Just tyInfo, foldr (uncurry Map.insert) mempty fieldTys) fields
          f <- convertExpr env ty ty'
          pure $ Bound.instantiate1 expr' f
        _ -> throwError $ ExpectedRecord ty
    _ -> do
      (expr', ty') <- inferExpr env expr
      f <- convertExpr env ty ty'
      pure $ Bound.instantiate1 expr' f

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
                 v' <- checkExpr env v vTy
                 pure (f, (v', vTy))
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
    Syntax.SelectFrom table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      pure (Syntax.SelectFrom table, Syntax.TQuery anyTypeInfo (_tiReadType info))
    Syntax.InsertInto value table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      value' <- checkExpr env value (_tiWriteType info)
      pure (Syntax.InsertInto value' table, Syntax.TQuery anyTypeInfo $ Syntax.TUnit anyTypeInfo)
    Syntax.InsertIntoReturning value table -> do
      language env Syntax.Postgresql
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      value' <- checkExpr env value (_tiWriteType info)
      pure (Syntax.InsertIntoReturning value' table, Syntax.TQuery anyTypeInfo (_tiReadType info))
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
              unless (tyInfo == tyInfo') $ error "type infos didn't match"
              pure (Syntax.Bind value' n $ Syntax.toScope2 rest', Syntax.TQuery tyInfo ty')
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
          f' <- checkExpr env' (Syntax.fromScope2 f) zTy
          pure (Syntax.FoldOptional z' (n, Syntax.toScope2 f') value', zTy)
        _ -> throwError $ ExpectedOptional valueTy
    Syntax.Many values
      | Vector.length values > 0 -> do
          (head', headTy) <- inferExpr env (Vector.head values)
          tail' <-
            traverse
              (\value -> checkExpr env value headTy)
              (Vector.tail values)
          let
            headAnn = Syntax.getTypeAnn headTy
            tyInfo =
              headAnn
              { _typeInfoOrigin = do
                  origin <- _typeInfoOrigin headAnn
                  case origin of
                    Row -> pure Rows
                    _ -> pure origin
              }
          pure
            ( Syntax.Many $ Vector.cons head' tail'
            , Syntax.TMany tyInfo headTy
            )
      | otherwise ->
          throwError $ Can'tInferExpr (_qeNames env <$> expr)
    Syntax.Int n -> pure (Syntax.Int n, Syntax.TInt anyTypeInfo)
    Syntax.Bool b -> pure (Syntax.Bool b, Syntax.TBool anyTypeInfo)
    Syntax.IfThenElse a b c -> do
      a' <- checkExpr env a $ Syntax.TBool anyTypeInfo
      (b', bTy) <- inferExpr env b
      c' <- checkExpr env c bTy
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
                Just . Syntax.toScope2 <$>
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
    Syntax.Project _ val field -> do
      (val', valTy) <- inferExpr env val
      case valTy of
        Syntax.TRecord _ fields ->
          case Vector.find ((field ==) . fst) fields of
            Nothing -> throwError $ MissingField valTy field
            Just (_, fieldTy) -> do
              let info = Info { _infoType = fieldTy }
              pure (Syntax.Project info val' field, fieldTy)
        _ -> throwError $ ExpectedRecord valTy
    Syntax.HasField record field -> do
      (record', recordTy) <- inferExpr env record
      case recordTy of
        Syntax.TRecord{} -> pure (Syntax.HasField record' field, Syntax.TBool anyTypeInfo)
        _ -> throwError $ ExpectedRecord recordTy

    Syntax.Var n -> pure (Syntax.Var n, _qeLocals env n)

    Syntax.AND a b -> do
      a' <- checkExpr env a $ Syntax.TBool anyTypeInfo
      b' <- checkExpr env b $ Syntax.TBool anyTypeInfo
      pure (Syntax.AND a' b', Syntax.TBool anyTypeInfo)
    Syntax.OR a b -> do
      a' <- checkExpr env a $ Syntax.TBool anyTypeInfo
      b' <- checkExpr env b $ Syntax.TBool anyTypeInfo
      pure (Syntax.OR a' b', Syntax.TBool anyTypeInfo)
    Syntax.EQ a b -> do
      (a', aTy) <- inferExpr env a
      b' <- checkExpr env b aTy
      pure (Syntax.EQ a' b', Syntax.TBool anyTypeInfo)
    Syntax.NOT a -> do
      a' <- checkExpr env a $ Syntax.TBool anyTypeInfo
      pure (Syntax.NOT a', Syntax.TBool anyTypeInfo)

data DeclEnv
  = DeclEnv
  { _deLanguage :: Language
  , _deTypes :: Map Text (Type TypeInfo)
  , _deTables :: Map Text TableInfo
  , _deGlobalVars :: Map Text (Type TypeInfo)
  , _deGlobalQueries :: Map Text (Type TypeInfo)
  }

data DeclError t t'
  = UnknownConstraint Text
  | ConstraintArgsMismatch Int Int
  | NotEnumerable (Type t')
  | MultiplePrimaryKeys
  | FieldNotInScope Text
  | FieldAlreadyDefined Text
  | TypeAlreadyDefined Text
  | TableAlreadyDefined Text
  | VariableAlreadyDefined Text
  | DuplicateArgument Text
  | TypeError (TypeError t)

isEnumerable :: Type () -> Bool
isEnumerable ty = ty `Set.member` tys
  where
    tys = [Syntax.TInt ()]

data TableItemState t
  = TableItemState
  { hasPrimaryKey :: Bool
  , fieldsSeen :: Map Text (Type t)
  }

checkTableItem ::
  ( MonadError (DeclError t t') m
  , MonadState (TableItemState t') m
  ) =>
  DeclEnv ->
  TableItem t' ->
  m (TableItem TypeInfo)
checkTableItem env item =
  case item of
    Syntax.Field name ty -> do
      m_ty <- gets (Map.lookup name . fieldsSeen)
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
          modify $ \s -> s { fieldsSeen = Map.insert name ty (fieldsSeen s) }
          pure $ Syntax.Field name ty'
        Just{} -> throwError $ FieldAlreadyDefined name
    Syntax.Constraint name args ->
      case name of
        "AUTO_INCREMENT" -> do
          let argsLength = Vector.length args
          unless (argsLength == 1) . throwError $ ConstraintArgsMismatch 1 argsLength
          let arg = args Vector.! 0
          argTy <-
            maybe (throwError $ FieldNotInScope arg) pure =<<
            gets (Map.lookup arg . fieldsSeen)
          unless (isEnumerable $ () <$ argTy) . throwError $ NotEnumerable argTy
          pure $ Syntax.Constraint name args
        "PK" -> do
          b <- gets hasPrimaryKey
          when b . throwError $ MultiplePrimaryKeys
          modify $ \s -> s { hasPrimaryKey = True }
          pure $ Syntax.Constraint name args
        _ -> throwError $ UnknownConstraint name

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
    Syntax.Table name items -> do
      case Map.lookup name (_deTables env) of
        Nothing -> pure ()
        Just{} -> throwError $ TableAlreadyDefined name
      items' <-
        evalStateT
          (traverse (checkTableItem env) items)
          (TableItemState False mempty)
      pure $ Syntax.Table name items'
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
      case Map.lookup name (_deGlobalVars env <> _deGlobalQueries env) of
        Just{} -> throwError $ VariableAlreadyDefined name
        Nothing -> do
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
          body' <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy'
          pure $ Syntax.Query name args' retTy' (Bound.toScope body')
    Syntax.Function name args retTy body ->
      case Map.lookup name (_deGlobalVars env <> _deGlobalQueries env) of
        Just{} -> throwError $ VariableAlreadyDefined name
        Nothing -> do
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
          body' <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy'
          pure $ Syntax.Function name args' retTy' (Bound.toScope body')

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
                Row -> pure Column
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
          Rows -> pure Row
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
  MonadError (TypeError t) m =>
  QueryEnv Void ->
  Vector (TableItem TypeInfo) ->
  m TableInfo
mkTableInfo env items = do
  readFields <- mkReadFields
  writeFields <- mkWriteFields readFields
  pure $
    TableInfo
    { _tiReadType = Syntax.TRecord (TypeInfo { _typeInfoOrigin = Just Row }) readFields
    , _tiWriteType = Syntax.TRecord (TypeInfo { _typeInfoOrigin = Just Row }) writeFields
    }
  where
    unaryConstraints =
      Vector.foldr
        (\case
          Syntax.Constraint n args
            | Vector.length args == 1 ->
                Map.insertWith
                  (<>)
                  (args Vector.! 0)
                  (Set.singleton n)
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
               if "AUTO_INCREMENT" `Set.member` cs
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
  m ()
checkDecls e decls = go e [0 .. Vector.length decls - 1]
  where
    go ::
      MonadError (DeclError t t') m =>
      DeclEnv ->
      [Int] ->
      m ()
    go _ [] = pure ()
    go env (ix:ixs) = do
      decl <- checkDecl env $ decls Vector.! ix
      case decl of
        Syntax.Type name val ->
          go (env { _deTypes = Map.insert name val (_deTypes env) }) ixs
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
          items' <- mapError TypeError $ mkTableInfo env' items
          go (env { _deTables = Map.insert name items' (_deTables env) }) ixs
        Syntax.Query name args retTy _ -> error "todo: add query to scope" name args retTy
        Syntax.Function name args retTy _ -> error "todo: add function to scope" name args retTy
