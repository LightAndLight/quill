{-# language OverloadedStrings #-}
module Quill.SQL where

import qualified Bound
import Bound.Var (unvar)
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
import Quill.Check (QueryEnv, resolveType)
import Quill.Syntax (Expr, Query, TableItem, Type)
import qualified Quill.Syntax as Syntax

columnType :: QueryEnv Void -> Type -> ByteString
columnType env ty =
  case ty of
    Syntax.TRecord{} -> error "todo: records as column types"
    Syntax.TBool -> "boolean NOT NULL"
    Syntax.TMany{} -> error "todo: many as column types???"
    Syntax.TOptional{} -> error "todo: many as column types???"
    Syntax.TQuery{} -> error "todo: many as column types???"
    Syntax.TName n ->
      case resolveType env n of
        Left{} -> undefined
        Right ty' -> columnType env ty'
    Syntax.TInt -> "int NOT NULL"

createTable ::
  QueryEnv Void ->
  Text ->
  Vector TableItem ->
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
      Vector TableItem ->
      ( [Text]
      , Map
          Text -- column name
          ( Type -- column type
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

-- kleisli composition
composeQuery ::
  Query Expr (Bound.Var () a) ->
  Query Expr (Bound.Var () a) ->
  Query Expr (Bound.Var () a)
composeQuery f g =
  Syntax.Bind g "__temp" $ Bound.F <$> f

query ::
  (a -> Builder.Builder) ->
  Query Expr a ->
  Builder.Builder
query f q =
  case q of
    Syntax.SelectFrom table ->
      "SELECT * FROM " <>
      Builder.byteString (encodeUtf8 table)
    Syntax.InsertInto value table ->
      "INSERT " <>
      expr f value <>
      " INTO " <>
      Builder.byteString (encodeUtf8 table) <>
      " RETURNING *"
    Syntax.Bind q' _ rest ->
      case q' of
        Syntax.Bind q'' n' rest' ->
          query f $
          Syntax.Bind q'' n' (composeQuery rest rest')
        Syntax.Return value ->
          query f (rest Bound.>>>= unvar (\() -> value) pure)
        _ ->
          query
            (unvar (\() -> parens $ query f q') f)
            rest
    Syntax.Return value -> expr f value

expr ::
  (a -> Builder.Builder) ->
  Expr a ->
  Builder.Builder
expr f e =
  case e of
    Syntax.For n value m_cond yield ->
      let
        f' = unvar (\() -> Builder.byteString $ encodeUtf8 n) f
      in
      "SELECT " <>
      (case Bound.fromScope yield of
         Syntax.Var (Bound.B ()) -> "*"
         _ -> expr f' (Bound.fromScope yield)
      ) <>
      " FROM " <>
      (case value of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         _ -> parens
      ) (expr f value) <>
      " AS " <>
      Builder.byteString (encodeUtf8 n) <>
      case m_cond of
        Nothing -> mempty
        Just cond ->
          " WHERE " <>
          expr f' (Bound.fromScope cond)
    Syntax.Name n ->
      error "todo: SQL.expr name" n
    Syntax.Project value field ->
      (case value of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         _ -> parens
      ) (expr f value) <>
      "." <>
      Builder.byteString (encodeUtf8 field)
    Syntax.Var n -> f n
    Syntax.Record fields ->
      fold . intersperse ", " $
      foldr ((:) . expr f . snd) [] fields
    Syntax.Int n -> Builder.stringUtf8 $ show n
    Syntax.Bool b -> if b then "true" else "false"
    Syntax.Many values ->
      fold . intersperse ", " $
      foldr ((:) . parens . expr f) [] values
    Syntax.AND a b ->
      (case a of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         Syntax.EQ{} -> id
         Syntax.OR{} -> id
         _ -> parens
      ) (expr f a) <>
      " AND " <>
      (case b of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         Syntax.EQ{} -> id
         Syntax.OR{} -> id
         _ -> parens
      ) (expr f b)
    Syntax.OR a b ->
      (case a of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         Syntax.EQ{} -> id
         _ -> parens
      ) (expr f a) <>
      " OR " <>
      (case b of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         Syntax.EQ{} -> id
         _ -> parens
      ) (expr f b)
    Syntax.EQ a b ->
      (case a of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         _ -> parens
      ) (expr f a) <>
      " = " <>
      (case b of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         Syntax.NOT{} -> id
         _ -> parens
      ) (expr f b)
    Syntax.NOT a ->
      "NOT " <>
      (case a of
         Syntax.Project{} -> id
         Syntax.Var{} -> id
         Syntax.Name{} -> id
         Syntax.Int{} -> id
         Syntax.Bool{} -> id
         _ -> parens
      ) (expr f a)
