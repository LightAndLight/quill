{-# language BangPatterns #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Quill.Check
  ( TableInfo(..)
  , QueryEnv(..)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (absurd)
import Quill.Syntax (Decl, Expr, Language, TableItem, Type)
import qualified Quill.Syntax as Syntax

data TypeError t
  = ExpectedRecord Type
  | ExpectedMany Type
  | ExpectedOptional Type
  | ExpectedQuery Type
  | MissingField Type Text
  | TypeMismatch Type Type
  | TypeNotInScope Text
  | TableNotInScope Text
  | VariableNotInScope Text
  | Can'tInferExpr (Expr t Text)
  | LanguageMismatch Language Language
  | DuplicateRecordFields
  deriving (Eq, Show)

data TableInfo
  = TableInfo
  { _tiReadType :: Type
  , _tiWriteType :: Type
  }

data QueryEnv a
  = QueryEnv
  { _qeLanguage :: Language
  , _qeNames :: a -> Text
  , _qeLocals :: a -> Type
  , _qeGlobalVars :: Map Text Type
  , _qeGlobalQueries :: Map Text Type
  , _qeTypes :: Map Text Type
  , _qeTables :: Map Text TableInfo
  }

resolveType ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Text ->
  m Type
resolveType env n =
  maybe
    (throwError $ TypeNotInScope n)
    pure
    (Map.lookup n $ _qeTypes env)

compose :: Monad f => Scope () f a -> Scope () f a -> Scope () f a
compose f (Bound.fromScope -> g) = Bound.toScope $ unvar (\() -> g) (pure . Bound.F) =<< Bound.fromScope f

insertAt ::
  Int ->
  Vector (Text, Type) ->
  (Text, Type) ->
  (Bound.Scope () (Expr t) a, Vector (Text, Type))
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
    keep (f, _) = (f, Syntax.Project (Syntax.Var (Bound.B ())) f)
    (prefix, suffix) = Vector.splitAt ix fields

convertFields ::
  forall t m a.
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Vector (Text, Type) ->
  Vector (Text, Type) ->
  m (Maybe (Bound.Scope () (Expr t) a))
convertFields env e a = do
  (res, _) <- go 0 a e a
  pure res
  where
    go ::
      Int ->
      Vector (Text, Type) ->
      Vector (Text, Type) ->
      Vector (Text, Type) ->
      m (Maybe (Bound.Scope () (Expr t) a), Vector (Text, Type))
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
                      let f' = Bound.toScope $ Syntax.Update field ("__temp", Bound.F <$> f) (Syntax.Var $ Bound.B ())
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
      , Bound.F <$>
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
      (Bound.toScope $ unvar Bound.B (Bound.F . Bound.F) <$> Bound.fromScope f)

convertExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Type ->
  Type ->
  m (Bound.Scope () (Expr t) a)
convertExpr env expected actual =
  case expected of
    Syntax.TName n -> do
      expected' <- resolveType env n
      convertExpr env expected' actual
    _ | Syntax.TName n <- actual -> do
      actual' <- resolveType env n
      convertExpr env expected actual'
    Syntax.TRecord fields ->
      case actual of
        Syntax.TRecord fields' -> do
          m_fields'' <- convertFields env fields fields'
          case m_fields'' of
            Nothing -> throwError $ TypeMismatch expected actual
            Just expr' -> pure expr'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TInt ->
      case actual of
        Syntax.TInt -> pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TUnit ->
      case actual of
        Syntax.TUnit -> pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TBool ->
      case actual of
        Syntax.TBool -> pure identity
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TQuery{} -> do
      case actual of
        Syntax.TQuery{} -> mapQuery <$> convertExpr env expected actual
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TMany a -> do
      case actual of
        Syntax.TMany a' -> mapMany <$> convertExpr env a a'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TOptional a -> do
      case actual of
        Syntax.TOptional a' -> mapOptional <$> convertExpr env a a'
        _ -> throwError $ TypeMismatch expected actual

language :: MonadError (TypeError t) m => QueryEnv a -> Language -> m ()
language env l =
  unless (l == _qeLanguage env) . throwError $
  LanguageMismatch l (_qeLanguage env)

checkType ::
  MonadError (TypeError t) m =>
  Type ->
  m ()
checkType ty =
  case ty of
    Syntax.TRecord fields ->
      let
        names = foldr (Set.insert . fst) mempty fields
      in
        if Set.size names == Vector.length fields
        then traverse_ (checkType . snd) fields
        else throwError DuplicateRecordFields
    Syntax.TUnit -> pure ()
    Syntax.TBool -> pure ()
    Syntax.TMany a -> checkType a
    Syntax.TQuery a -> checkType a
    Syntax.TOptional a -> checkType a
    Syntax.TName{} -> pure ()
    Syntax.TInt -> pure ()

mapQuery :: Bound.Scope () (Expr t) a -> Bound.Scope () (Expr t) a
mapQuery f =
  Bound.toScope . Syntax.Bind (Syntax.Var $ Bound.B ()) "__temp" .
  Bound.toScope . Syntax.Return $
  unvar Bound.B (Bound.F . Bound.F) <$> Bound.fromScope f

checkExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Expr t a ->
  Type ->
  m (Expr t a)
checkExpr env expr ty = do
  case expr of
    Syntax.Many values ->
      case ty of
        Syntax.TMany ty' -> Syntax.Many <$> traverse (\e -> checkExpr env e ty') values
        _ -> throwError $ ExpectedMany ty
    Syntax.None ->
      case ty of
        Syntax.TOptional{} -> pure expr
        _ -> throwError $ ExpectedOptional ty
    Syntax.Record fields ->
      case ty of
        Syntax.TRecord fieldTys -> do
          (expr', ty') <- inferRecord env (foldr (uncurry Map.insert) mempty fieldTys) fields
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
  Map Text Type ->
  Vector (Text, Expr t a) ->
  m (Expr t a, Type)
inferRecord env hints fields =
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
        , Syntax.TRecord $ (\(n, (_, ty)) -> (n, ty)) <$> results
        )
    else throwError DuplicateRecordFields

inferExpr ::
  MonadError (TypeError t) m =>
  QueryEnv a ->
  Expr t a ->
  m (Expr t a, Type)
inferExpr env expr =
  case expr of
    Syntax.SelectFrom table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      pure (Syntax.SelectFrom table, Syntax.TQuery (_tiReadType info))
    Syntax.InsertInto value table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      value' <- checkExpr env value (_tiWriteType info)
      pure (Syntax.InsertInto value' table, Syntax.TQuery Syntax.TUnit)
    Syntax.InsertIntoReturning value table -> do
      language env Syntax.Postgresql
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      value' <- checkExpr env value (_tiWriteType info)
      pure (Syntax.InsertIntoReturning value' table, Syntax.TQuery (_tiReadType info))
    Syntax.Bind value n rest -> do
      (value', valueTy) <- inferExpr env value
      case valueTy of
        Syntax.TQuery ty -> do
          (rest', restTy) <-
            inferExpr
              (env
               { _qeNames = unvar (\() -> n) (_qeNames env)
               , _qeLocals = unvar (\() -> ty) (_qeLocals env)
               }
              )
              (Bound.fromScope rest)
          case restTy of
            Syntax.TQuery ty' ->
              pure (Syntax.Bind value' n $ Bound.toScope rest', Syntax.TQuery ty')
            _ -> throwError $ ExpectedQuery restTy
        _ -> throwError $ ExpectedQuery valueTy
    Syntax.Return value -> do
      (value', valueTy) <- inferExpr env value
      pure (Syntax.Return value', Syntax.TQuery valueTy)
    Syntax.Some a -> do
      (a', aTy) <- inferExpr env a
      pure (Syntax.Some a', Syntax.TOptional aTy)
    Syntax.None -> throwError $ Can'tInferExpr (_qeNames env <$> expr)
    Syntax.FoldOptional z (n, f) value -> do
      (value', valueTy) <- inferExpr env value
      case valueTy of
        Syntax.TOptional a -> do
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
          f' <- checkExpr env' (Bound.fromScope f) zTy
          pure (Syntax.FoldOptional z' (n, Bound.toScope f') value', zTy)
        _ -> throwError $ ExpectedOptional valueTy
    Syntax.Many values
      | Vector.length values > 0 -> do
          (head', headTy) <- inferExpr env (Vector.head values)
          tail' <-
            traverse
              (\value -> checkExpr env value headTy)
              (Vector.tail values)
          pure (Syntax.Many $ Vector.cons head' tail', Syntax.TMany headTy)
      | otherwise ->
          throwError $ Can'tInferExpr (_qeNames env <$> expr)
    Syntax.Int{} -> pure (expr, Syntax.TInt)
    Syntax.Bool{} -> pure (expr, Syntax.TBool)
    Syntax.IfThenElse a b c -> do
      a' <- checkExpr env a Syntax.TBool
      (b', bTy) <- inferExpr env b
      c' <- checkExpr env c bTy
      pure (Syntax.IfThenElse a' b' c', bTy)
    Syntax.For n value m_cond yield -> do
      (value', valTy) <- inferExpr env value
      case valTy of
        Syntax.TMany itemTy -> do
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
                Just . Bound.toScope <$>
                checkExpr
                  env'
                  (Bound.fromScope cond)
                  Syntax.TBool
          (yield', yieldTy) <-
            inferExpr
              env'
              (Bound.fromScope yield)
          pure (Syntax.For n value' m_cond' $ Bound.toScope yield', Syntax.TMany yieldTy)
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
        Syntax.TRecord fields -> do
          case Vector.find ((field ==) . fst) fields of
            Nothing -> do
              (value', valueTy) <- inferExpr env value
              pure
                ( Syntax.Extend field value' rest'
                , Syntax.TRecord $ Vector.cons (field, valueTy) fields
                )
            Just{} -> throwError $ DuplicateRecordFields
        _ -> throwError $ ExpectedRecord restTy
    Syntax.Update field (n, fun) record -> do
      (record', recordTy) <- inferExpr env record
      case recordTy of
        Syntax.TRecord fields ->
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
              (fun', fieldTy') <- inferExpr env' (Bound.fromScope fun)
              pure
                ( Syntax.Update field (n, Bound.toScope fun') record'
                , Syntax.TRecord $
                  (\(f, t) -> if f == field then (f, fieldTy') else (f, t)) <$>
                  fields
                )
        _ -> throwError $ ExpectedRecord recordTy
    Syntax.Record fields -> inferRecord env mempty fields
    Syntax.Project val field -> do
      (val', valTy) <- inferExpr env val
      case valTy of
        Syntax.TRecord fields ->
          case Vector.find ((field ==) . fst) fields of
            Nothing -> throwError $ MissingField valTy field
            Just (_, fieldTy) -> pure (Syntax.Project val' field, fieldTy)
        _ -> throwError $ ExpectedRecord valTy
    Syntax.HasField record field -> do
      (record', recordTy) <- inferExpr env record
      case recordTy of
        Syntax.TRecord{} -> pure (Syntax.HasField record' field, Syntax.TBool)
        _ -> throwError $ ExpectedRecord recordTy

    Syntax.Var n -> pure (Syntax.Var n, _qeLocals env n)

    Syntax.AND a b -> do
      a' <- checkExpr env a Syntax.TBool
      b' <- checkExpr env b Syntax.TBool
      pure (Syntax.AND a' b', Syntax.TBool)
    Syntax.OR a b -> do
      a' <- checkExpr env a Syntax.TBool
      b' <- checkExpr env b Syntax.TBool
      pure (Syntax.OR a' b', Syntax.TBool)
    Syntax.EQ a b -> do
      (a', aTy) <- inferExpr env a
      b' <- checkExpr env b aTy
      pure (Syntax.EQ a' b', Syntax.TBool)
    Syntax.NOT a -> do
      a' <- checkExpr env a Syntax.TBool
      pure (Syntax.NOT a', Syntax.TBool)

data DeclEnv
  = DeclEnv
  { _deLanguage :: Language
  , _deTypes :: Map Text Type
  , _deTables :: Map Text TableInfo
  , _deGlobalVars :: Map Text Type
  , _deGlobalQueries :: Map Text Type
  }

data DeclError t
  = UnknownConstraint Text
  | ConstraintArgsMismatch Int Int
  | NotEnumerable Type
  | MultiplePrimaryKeys
  | FieldNotInScope Text
  | FieldAlreadyDefined Text
  | TypeAlreadyDefined Text
  | TableAlreadyDefined Text
  | VariableAlreadyDefined Text
  | DuplicateArgument Text
  | TypeError (TypeError t)

isEnumerable :: Type -> Bool
isEnumerable ty = ty `Set.member` tys
  where
    tys = [Syntax.TInt]

data TableItemState
  = TableItemState
  { hasPrimaryKey :: Bool
  , fieldsSeen :: Map Text Type
  }

checkTableItem ::
  ( MonadError (DeclError t) m
  , MonadState TableItemState m
  ) =>
  DeclEnv ->
  TableItem ->
  m ()
checkTableItem _ item =
  case item of
    Syntax.Field name ty -> do
      m_ty <- gets (Map.lookup name . fieldsSeen)
      case m_ty of
        Nothing -> do
          mapError TypeError (checkType ty)
          modify $ \s -> s { fieldsSeen = Map.insert name ty (fieldsSeen s) }
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
          unless (isEnumerable argTy) . throwError $ NotEnumerable argTy
        "PK" -> do
          b <- gets hasPrimaryKey
          when b . throwError $ MultiplePrimaryKeys
          modify $ \s -> s { hasPrimaryKey = True }
        _ -> throwError $ UnknownConstraint name

checkDeclArg ::
  ( MonadError (DeclError t) m
  , MonadState (Set Text) m
  ) =>
  DeclEnv ->
  (Text, Type) ->
  m Type
checkDeclArg _ (name, ty) = do
  seen <- gets $ Set.member name
  when seen . throwError $ DuplicateArgument name
  ty <$ mapError TypeError (checkType ty)

mapError :: MonadError b m => (a -> b) -> ExceptT a m x -> m x
mapError f = (either throwError pure =<<) . runExceptT . withExceptT f

checkDecl ::
  MonadError (DeclError t) m =>
  DeclEnv ->
  Decl t ->
  m (Decl t)
checkDecl env decl =
  case decl of
    Syntax.Table name items -> do
      case Map.lookup name (_deTables env) of
        Nothing -> pure ()
        Just{} -> throwError $ TableAlreadyDefined name
      evalStateT
        (traverse_ (checkTableItem env) items)
        (TableItemState False mempty)
      pure decl
    Syntax.Type name ty ->
      case Map.lookup name (_deTypes env) of
        Nothing -> decl <$ mapError TypeError (checkType ty)
        Just{} -> throwError $ TypeAlreadyDefined name
    Syntax.Query name args retTy body ->
      case Map.lookup name (_deGlobalVars env <> _deGlobalQueries env) of
        Just{} -> throwError $ VariableAlreadyDefined name
        Nothing -> do
          argTys <- evalStateT (traverse (checkDeclArg env) args) mempty
          case retTy of
            Syntax.TQuery{} -> pure ()
            _ -> throwError . TypeError $ ExpectedQuery retTy
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = unvar (fst . (args Vector.!)) absurd
              , _qeLocals = unvar (argTys Vector.!) absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }

          body' <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy
          pure $ Syntax.Query name args retTy (Bound.toScope body')
    Syntax.Function name args retTy body ->
      case Map.lookup name (_deGlobalVars env <> _deGlobalQueries env) of
        Just{} -> throwError $ VariableAlreadyDefined name
        Nothing -> do
          argTys <- evalStateT (traverse (checkDeclArg env) args) mempty
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeNames = unvar (fst . (args Vector.!)) absurd
              , _qeLocals = unvar (argTys Vector.!) absurd
              , _qeGlobalVars = _deGlobalVars env
              , _qeGlobalQueries = _deGlobalQueries env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }

          body' <- mapError TypeError $ checkExpr queryEnv (Bound.fromScope body) retTy
          pure $ Syntax.Function name args retTy (Bound.toScope body')

mkTableInfo :: Vector TableItem -> TableInfo
mkTableInfo items =
  TableInfo
  { _tiReadType = Syntax.TRecord readFields
  , _tiWriteType = Syntax.TRecord writeFields
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
    readFields =
      Vector.mapMaybe
        (\case
          Syntax.Field n ty -> Just (n, ty)
          _ -> Nothing
        )
        items
    writeFields =
      fmap
        (\(n, ty) ->
           case Map.lookup n unaryConstraints of
             Nothing -> (n, ty)
             Just cs ->
               if "AUTO_INCREMENT" `Set.member` cs
               then (n, Syntax.TOptional ty)
               else (n, ty)
        )
        readFields

checkDecls ::
  MonadError (DeclError t) m =>
  DeclEnv ->
  Vector (Decl t) ->
  m ()
checkDecls e decls = go e [0 .. Vector.length decls - 1]
  where
    go _ [] = pure ()
    go env (ix:ixs) = do
      decl <- checkDecl env $ decls Vector.! ix
      case decl of
        Syntax.Type name val ->
          go (env { _deTypes = Map.insert name val (_deTypes env) }) ixs
        Syntax.Table name items ->
          go (env { _deTables = Map.insert name (mkTableInfo items) (_deTables env) }) ixs
        Syntax.Query name args retTy _ -> error "todo: add query to scope" name args retTy
        Syntax.Function name args retTy _ -> error "todo: add function to scope" name args retTy
