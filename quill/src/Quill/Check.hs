{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
module Quill.Check
  ( TableInfo(..)
  , QueryEnv(..)
  , resolveType
  , TypeError(..)
  , checkQuery
  , inferQuery
  , checkExpr
  , inferExpr
  , DeclEnv(..)
  , DeclError(..)
  , checkDecl
  , checkDecls
  )
where

import qualified Bound
import Bound.Var (unvar)
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, throwError, runExceptT, withExceptT)
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
import Quill.Syntax (Decl, Expr, Language, Query, TableItem, Type)
import qualified Quill.Syntax as Syntax

data TypeError
  = ExpectedRecord Type
  | ExpectedMany Type
  | ExpectedQuery Type
  | MissingField Type Text
  | TypeMismatch Type Type
  | TypeNotInScope Text
  | TableNotInScope Text
  | VariableNotInScope Text
  | Can'tInferQuery (Query Expr Text)
  | Can'tInferExpr (Expr Text)
  | LanguageMismatch Language Language

data TableInfo
  = TableInfo
  { _tiReadType :: Type
  , _tiWriteType :: Type
  }

data QueryEnv a
  = QueryEnv
  { _qeLanguage :: Language
  , _qeVarNames :: a -> Text
  , _qeLocals :: a -> Type
  , _qeGlobals :: Map Text Type
  , _qeTypes :: Map Text Type
  , _qeTables :: Map Text TableInfo
  }

resolveType ::
  MonadError TypeError m =>
  QueryEnv a ->
  Text ->
  m Type
resolveType env n =
  maybe
    (throwError $ TypeNotInScope n)
    pure
    (Map.lookup n $ _qeTypes env)

convert ::
  MonadError TypeError m =>
  QueryEnv a ->
  Type ->
  Type ->
  m ()
convert env expected actual =
  case expected of
    Syntax.TName n -> do
      expected' <- resolveType env n
      convert env expected' actual
    _ | Syntax.TName n <- actual -> do
      actual' <- resolveType env n
      convert env expected actual'
    Syntax.TRecord fields ->
      case actual of
        Syntax.TRecord fields'
          | Vector.length fields == Vector.length fields' -> do
              traverse_
                (\((n, ty), (n', ty')) -> do
                    unless (n == n') . throwError $ TypeMismatch expected actual
                    convert env ty ty'
                )
                (Vector.zip fields fields')
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TInt ->
      case actual of
        Syntax.TInt -> pure ()
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TUnit ->
      case actual of
        Syntax.TUnit -> pure ()
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TBool ->
      case actual of
        Syntax.TBool -> pure ()
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TMany a -> do
      case actual of
        Syntax.TMany a' -> convert env a a'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TQuery a -> do
      case actual of
        Syntax.TQuery a' -> convert env a a'
        _ -> throwError $ TypeMismatch expected actual
    Syntax.TOptional a -> do
      case actual of
        Syntax.TOptional a' -> convert env a a'
        _ -> throwError $ TypeMismatch expected actual

language :: MonadError TypeError m => QueryEnv a -> Language -> m ()
language env l =
  unless (l == _qeLanguage env) . throwError $
  LanguageMismatch l (_qeLanguage env)

checkQuery ::
  MonadError TypeError m =>
  QueryEnv a ->
  Query Expr a ->
  Type ->
  m ()
checkQuery env query ty = do
  ty' <- inferQuery env query
  convert env ty ty'

inferQuery ::
  MonadError TypeError m =>
  QueryEnv a ->
  Query Expr a ->
  m Type
inferQuery env query =
  case query of
    Syntax.SelectFrom table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      pure $ Syntax.TQuery (_tiReadType info)
    Syntax.InsertInto value table -> do
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      checkExpr env value (_tiWriteType info)
      pure $ Syntax.TQuery Syntax.TUnit
    Syntax.InsertIntoReturning value table -> do
      language env Syntax.Postgresql
      info <-
        maybe (throwError $ TableNotInScope table) pure $
        Map.lookup table (_qeTables env)
      checkExpr env value (_tiWriteType info)
      pure $ Syntax.TQuery (_tiReadType info)
    Syntax.Bind value n rest -> do
      valueTy <- inferQuery env value
      case valueTy of
        Syntax.TQuery ty -> do
          restTy <-
            inferQuery
              (env
               { _qeVarNames = unvar (\() -> n) (_qeVarNames env)
               , _qeLocals = unvar (\() -> ty) (_qeLocals env)
               }
              )
              rest
          case restTy of
            Syntax.TQuery ty' ->
              pure $ Syntax.TQuery ty'
            _ -> throwError $ ExpectedQuery restTy
        _ -> throwError $ ExpectedQuery valueTy
    Syntax.Return value ->
      Syntax.TQuery <$> inferExpr env value

checkExpr ::
  MonadError TypeError m =>
  QueryEnv a ->
  Expr a ->
  Type ->
  m ()
checkExpr env expr ty = do
  case expr of
    Syntax.Many values | Vector.length values == 0 -> pure ()
    _ -> do
      ty' <- inferExpr env expr
      convert env ty ty'

inferExpr ::
  MonadError TypeError m =>
  QueryEnv a ->
  Expr a ->
  m Type
inferExpr env expr =
  case expr of
    Syntax.Many values
      | Vector.length values > 0 -> do
          headTy <- inferExpr env (Vector.head values)
          traverse_
            (\value -> checkExpr env value headTy)
            (Vector.tail values)
          pure headTy
      | otherwise -> throwError $ Can'tInferExpr (_qeVarNames env <$> expr)
    Syntax.Int{} -> pure Syntax.TInt
    Syntax.Bool{} -> pure Syntax.TBool
    Syntax.For n value m_cond yield -> do
      valTy <- inferExpr env value
      case valTy of
        Syntax.TMany itemTy -> do
          let
            env' =
              env
              { _qeVarNames =
                  unvar (\() -> n) (_qeVarNames env)
              , _qeLocals =
                  unvar
                    (\() -> itemTy)
                    (_qeLocals env)
              }
          case m_cond of
            Nothing -> pure ()
            Just cond ->
              checkExpr
                env'
                (Bound.fromScope cond)
                Syntax.TBool
          Syntax.TMany <$>
            inferExpr
              env'
              (Bound.fromScope yield)
        _ -> throwError $ ExpectedMany valTy
    Syntax.Name n ->
      maybe
        (throwError $ VariableNotInScope n)
        pure
        (Map.lookup n $ _qeGlobals env)
    Syntax.Record fields ->
      Syntax.TRecord <$>
     (traverse.traverse) (inferExpr env) fields
    Syntax.Project val field -> do
      valTy <- inferExpr env val
      case valTy of
        Syntax.TRecord fields ->
          case Vector.find ((field ==) . fst) fields of
            Nothing -> throwError $ MissingField valTy field
            Just (_, fieldTy) -> pure fieldTy
        _ -> throwError $ ExpectedRecord valTy
    Syntax.Var n -> pure $ _qeLocals env n

    Syntax.AND a b -> do
      checkExpr env a Syntax.TBool
      checkExpr env b Syntax.TBool
      pure Syntax.TBool
    Syntax.OR a b -> do
      checkExpr env a Syntax.TBool
      checkExpr env b Syntax.TBool
      pure Syntax.TBool
    Syntax.EQ a b -> do
      aTy <- inferExpr env a
      checkExpr env b aTy
      pure Syntax.TBool
    Syntax.NOT a ->
      Syntax.TBool <$ checkExpr env a Syntax.TBool

data DeclEnv
  = DeclEnv
  { _deLanguage :: Language
  , _deTypes :: Map Text Type
  , _deTables :: Map Text TableInfo
  , _deGlobals :: Map Text Type
  }

data DeclError
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
  | TypeError TypeError

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
  ( MonadError DeclError m
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
        Nothing ->
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
  ( MonadError DeclError m
  , MonadState (Set Text) m
  ) =>
  DeclEnv ->
  (Text, Type) ->
  m Type
checkDeclArg _ (name, ty) = do
  seen <- gets $ Set.member name
  when seen . throwError $ DuplicateArgument name
  pure ty

checkDecl ::
  MonadError DeclError m =>
  DeclEnv ->
  Decl ->
  m Decl
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
    Syntax.Type name _ ->
      case Map.lookup name (_deTypes env) of
        Nothing -> pure decl
        Just{} -> throwError $ TypeAlreadyDefined name
    Syntax.Query name args retTy body ->
      case Map.lookup name (_deGlobals env) of
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
              , _qeVarNames = unvar (fst . (args Vector.!)) absurd
              , _qeLocals = unvar (argTys Vector.!) absurd
              , _qeGlobals = _deGlobals env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }

          (either throwError pure =<<) . runExceptT . withExceptT TypeError $
            checkQuery queryEnv body retTy
          pure decl
    Syntax.Function name args retTy body ->
      case Map.lookup name (_deGlobals env) of
        Just{} -> throwError $ VariableAlreadyDefined name
        Nothing -> do
          argTys <- evalStateT (traverse (checkDeclArg env) args) mempty
          let
            queryEnv =
              QueryEnv
              { _qeLanguage = _deLanguage env
              , _qeVarNames = unvar (fst . (args Vector.!)) absurd
              , _qeLocals = unvar (argTys Vector.!) absurd
              , _qeGlobals = _deGlobals env
              , _qeTypes = _deTypes env
              , _qeTables = _deTables env
              }

          (either throwError pure =<<) . runExceptT . withExceptT TypeError $
            checkExpr queryEnv (Bound.fromScope body) retTy
          pure decl

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
  MonadError DeclError m =>
  DeclEnv ->
  Vector Decl ->
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
