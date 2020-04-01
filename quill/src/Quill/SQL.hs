{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ViewPatterns #-}
module Quill.SQL where

import qualified Bound
import Bound.Var (unvar)
import Control.Lens.Cons (_Cons, _last)
import Control.Lens.Fold ((^?))
import Control.Lens.Setter (over)
import Control.Monad (join)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Quill.Check (Info(..), QueryEnv, TypeInfo(..), resolveType)
import qualified Quill.Check as Check
import Quill.Syntax (TableItem, Type)
import qualified Quill.Syntax as Syntax

columnType :: QueryEnv Void -> Type TypeInfo -> ByteString
columnType env ty =
  case ty of
    Syntax.TRecord _ _ -> error "todo: records as column types"
    Syntax.TUnit _ -> "blob NOT NULL"
    Syntax.TBool _ -> "boolean NOT NULL"
    Syntax.TMany _ _ -> error "todo: many as column types???"
    Syntax.TOptional _ _ -> error "todo: optional as column types???"
    Syntax.TQuery _ _ -> error "todo: query as column types???"
    Syntax.TName _ n ->
      case resolveType env n of
        Left{} -> undefined
        Right ty' -> columnType env ty'
    Syntax.TInt _ -> "int NOT NULL"

createTable ::
  QueryEnv Void ->
  Text ->
  Vector (TableItem TypeInfo) ->
  Lazy.ByteString
createTable env tableName items =
  Builder.toLazyByteString $
  "CREATE TABLE " <> Builder.byteString (encodeUtf8 tableName) <> "(\n" <>
  foldMap
    (\colName ->
       case Map.lookup colName colInfos of
         Nothing -> undefined
         Just (ty, cons) ->
           Builder.byteString (encodeUtf8 colName) <>
           " " <>
           Builder.byteString (columnType env ty) <>
           " " <>
           fold
             (intersperse " " $
              Builder.byteString . encodeUtf8 <$> cons
             )
    )
    colNames <>
  fold
    (intersperse ",\n" $
     (\(n, args) ->
        Builder.byteString (encodeUtf8 n) <> "(" <>
        fold
          (intersperse "," $
           foldr
             ((:) . Builder.byteString . encodeUtf8)
             []
             args
          ) <>
        ")"
     ) <$> constraints
    ) <>
  "\n)"
  where
    (colNames, colInfos, constraints) = gatherConstraints items

    gatherConstraints ::
      Vector (TableItem TypeInfo) ->
      ( [Text]
      , Map
          Text -- column name
          ( Type TypeInfo -- column type
          , [Text] -- unary constraints
          )
      , [(Text, Vector Text)] -- higher arity constraints
      )
    gatherConstraints =
      foldr
        (\i (cols, info, constrs) ->
           case i of
             Syntax.Field name ty ->
               ( name : cols
               , Map.insert name (ty, []) info
               , constrs
               )
             Syntax.Constraint name args
               | Vector.length args == 1 ->
                   ( cols
                   , Map.adjust
                       (\(ty, cs) -> (ty, name : cs))
                       (args Vector.! 0)
                       info
                   , constrs
                   )
               | otherwise ->
                 ( cols
                 , info
                 , (name, args) : constrs
                 )
        )
        ([], Map.empty, [])

parens :: Builder.Builder -> Builder.Builder
parens a = "(" <> a <> ")"

data Expr
  = Subquery Query
  | Null
  | Var Text
  | Project Expr Text
  | Row Expr (Vector Expr)
  | List (Vector Expr)
  | Int Int
  | Bool Bool
  | And Expr Expr
  | Or Expr Expr
  | Eq Expr Expr
  | Not Expr

data Selection
  = Star
  | Values Expr (Vector Expr)

data Query
  = SelectFrom
      Selection -- * or a list of variables/projections
      Expr -- source
      (Maybe Text) -- AS name
      (Maybe Expr) -- WHERE expr
  | InsertInto
      Text -- table name
      (Vector Text) -- column names
      (Vector Expr) -- values
  | InsertIntoReturning
      Text -- table name
      (Vector Text) -- column names
      (Vector Expr) -- values
      Selection -- selection

row ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either () (Vector Expr)
row env f e = do
  e' <- expr env f e
  pure $
    case e' of
      Row e es -> Vector.cons e es
      _ -> [e']

query ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either () Query
query env f e =
  case e of
    Syntax.Info _ a -> query env f a
    Syntax.SelectFrom table ->
      pure $ SelectFrom Star (Var table) Nothing Nothing
    Syntax.InsertIntoReturning value table -> do
      values <- row env f value
      tableInfo <-
        maybe (error "SQL.query - table not found") pure $
        Map.lookup table (Check._deTables env)
      pure $
        InsertIntoReturning
        table
        (Check._tiFieldNames tableInfo >>= \(_, (f, fs)) -> Vector.cons f fs)
        values
        Star
    Syntax.InsertInto value table -> do
      values <- row env f value
      tableInfo <-
        maybe (error "SQL.query - table not found") pure $
        Map.lookup table (Check._deTables env)
      pure $
        InsertInto
        table
        (Check._tiFieldNames tableInfo >>= \(_, (f, fs)) -> Vector.cons f fs)
        values
    Syntax.Bind q' _ rest -> do
      q'' <- expr env f q'
      query
        env
        (unvar (\() -> q'') f)
        (Syntax.fromScope2 rest)
    Syntax.For n value m_cond yield -> do
      let f' = unvar (\() -> Var n) f
      value' <- expr env f value
      yield' <- expr env f' (Syntax.fromScope2 yield)
      m_cond' <-
        case m_cond of
          Nothing -> pure Nothing
          Just cond -> Just <$> expr env f' (Syntax.fromScope2 cond)
      pure $
        SelectFrom
          (case Syntax.fromScope2 yield of
             Syntax.Var (Bound.B ()) -> Star
             _ ->
               case yield' of
                 Row v vs -> Values v vs
                 _ -> Values yield' []
          )
          value'
          (Just n)
          m_cond'
    _ -> Left ()

expr ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either () Expr
expr env f e =
  case e of
    Syntax.Info _ a -> expr env f a
    Syntax.Return value -> expr env f value
    Syntax.Update{} -> error "SQL.expr env update"
    Syntax.Extend{} -> error "SQL.expr env extend"
    Syntax.Some{} -> error "SQL.expr env some"
    Syntax.None -> error "SQL.expr env none"
    Syntax.FoldOptional{} -> error "SQL.expr env foldoptional"
    Syntax.Name n ->
      error "todo: SQL.expr env name" n
    Syntax.HasField value field ->
      error "todo: SQL.expr env hasfield" value field
    Syntax.Project value field ->
      case Syntax.getAnn value of
        Nothing -> error $ "SQL.expr env project - no ann for value: " <>  show value
        Just valueInfo ->
          let
            valueType = _infoType valueInfo
            valueTypeInfo = Syntax.getTypeAnn valueType
          in
            case Check._typeInfoOrigin valueTypeInfo of
              Nothing -> error "SQL.expr env project - unknown origin for value"
              Just valueOrigin -> do
                value' <- expr env f value
                case valueOrigin of
                  Check.Row table ->
                     case Map.lookup table (Check._deTables env) of
                       Nothing -> error $ "SQL.expr project - table not found"
                       Just tableInfo ->
                         case Vector.find ((field ==) . fst) (Check._tiFieldNames tableInfo) of
                           Nothing -> error $ "SQL.expr project - fieldNames not found"
                           Just (_, (f, fs)) ->
                             pure $
                             if Vector.null fs
                             then Project value' f
                             else Row (Project value' f) ((value' `Project`) <$> fs)
                  Check.Column -> _
                  _ -> error $ "SQL.expr project - invalid origin " <> show valueOrigin
    Syntax.Var n -> pure $ f n
    Syntax.Record fields -> do
      fields <- join <$> traverse (row env f . snd) fields
      pure $
        case fields ^? _Cons of
          Nothing -> Null
          Just (f, fs) -> Row f fs
    Syntax.Int n -> pure $ Int n
    Syntax.Bool b -> pure $ Bool b
    Syntax.IfThenElse a b c -> error "SQL.expr env ifthenelse" a b c
    Syntax.Many values -> List <$> traverse (expr env f) values
    Syntax.AND a b -> And <$> expr env f a <*> expr env f b
    Syntax.OR a b -> Or <$> expr env f a <*> expr env f b
    Syntax.EQ a b -> Eq <$> expr env f a <*> expr env f b
    Syntax.NOT a -> Not <$> expr env f a
    _ -> Subquery <$> query env f e
