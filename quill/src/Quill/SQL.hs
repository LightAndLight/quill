{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ViewPatterns #-}
module Quill.SQL
  ( Expr(..)
  , Query(..)
  , expr
  , query
  , CompileError(..)
  , compileExpr
  , compileQuery
  , compileTable
  , compileType
  )
where

import qualified Capnp.Gen.Table.Pure as SQL
  ( Column(..)
  , Constraint(..)
  , Table(Table)
  , Other(Other)
  )
import qualified Bound
import Bound.Var (unvar)
import Control.Lens.Cons (_Cons)
import Control.Lens.Fold ((^?))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Builder as Builder
import Data.Foldable (fold, foldl')
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Quill.Check (Info(..))
import qualified Quill.Check as Check
import Quill.Syntax (Type)
import qualified Quill.Syntax as Syntax

data CompileError
  = ExpectedQuery (Syntax.Expr Info String)
  deriving (Show)

parens :: Builder.Builder -> Builder.Builder
parens a = "(" <> a <> ")"

data Expr
  = Subquery Query
  | Null
  | Var Text
  | Project Expr Text
  | Row (Vector (Text, Expr))
  | List (Vector Expr)
  | Int Int
  | Bool Bool
  | And Expr Expr
  | Or Expr Expr
  | Eq Expr Expr
  | Not Expr

compileExpr :: Expr -> Builder.Builder
compileExpr e =
  case e of
    Subquery q -> compileQuery q
    Int n -> Builder.byteString . Char8.pack $ show n
    Bool b -> if b then "true" else "false"
    And a b -> compileExpr a <> " AND " <> compileExpr b
    Or a b -> compileExpr a <> " OR " <> compileExpr b
    Eq a b -> compileExpr a <> " = " <> compileExpr b
    Not a -> "NOT " <> compileExpr a
    Null -> "null"
    Var n -> Builder.byteString $ encodeUtf8 n
    Project value field -> compileExpr value <> "." <> Builder.byteString (encodeUtf8 field)
    Row values ->
      fold . intersperse ", " $
      foldr ((:) . compileExpr . snd) [] values
    List values ->
      fold . intersperse ", " $
      foldr
        (\value ->
           (:) $
           (case value of
             Row{} -> parens
             _ -> id
           ) (compileExpr value)
        )
        []
        values

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

compileSelection :: Selection -> Builder.Builder
compileSelection sel =
  case sel of
    Star -> "*"
    Values v vs -> compileExpr v <> foldMap ((", " <>) . compileExpr) vs

compileQuery :: Query -> Builder.Builder
compileQuery q =
  case q of
    SelectFrom sel src m_alias m_cond ->
      "SELECT " <> compileSelection sel <> " FROM " <>
      (case src of
         Subquery{} -> parens
         _ -> id
      ) (compileExpr src) <>
      foldMap (\alias -> " AS " <> Builder.byteString (encodeUtf8 alias)) m_alias <>
      foldMap (\cond -> " WHERE " <> compileExpr cond) m_cond
    InsertInto table cols vals ->
      "INSERT INTO " <>
      Builder.byteString (encodeUtf8 table) <>
      parens (fold . intersperse ", " $ foldr ((:) . Builder.byteString . encodeUtf8) [] cols) <>
      " VALUES " <>
      parens (fold . intersperse ", " $ foldr ((:) . compileExpr) [] vals)
    InsertIntoReturning table cols vals sel ->
      "INSERT INTO " <>
      Builder.byteString (encodeUtf8 table) <>
      parens (fold . intersperse ", " $ foldr ((:) . Builder.byteString . encodeUtf8) [] cols) <>
      "VALUES " <>
      parens (fold . intersperse ", " $ foldr ((:) . compileExpr) [] vals) <>
      " RETURNING " <> compileSelection sel

row ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either CompileError (Vector Text, Vector Expr)
row env f e = do
  e' <- expr env f e
  go "" e'
  where
    go :: Text -> Expr -> Either CompileError (Vector Text, Vector Expr)
    go !prefix e' =
      case e' of
        Row es ->
          (\es' -> (es' >>= fst, es' >>= snd)) <$>
          traverse (\(n, v) -> go (if Text.null prefix then n else prefix <> "_" <> n) v) es
        _ -> pure ([prefix], [e'])

query ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either CompileError Query
query env f e =
  case e of
    Syntax.Info _ a -> query env f a
    Syntax.SelectFrom table ->
      pure $ SelectFrom Star (Var table) Nothing Nothing
    Syntax.InsertIntoReturning value table -> do
      rowRes <- row env f value
      let
        (columns, values) =
          case rowRes of
            (fields, es) ->
              ( fields
              , es
              )
      pure $
        InsertIntoReturning
        table
        columns
        values
        Star
    Syntax.InsertInto value table -> do
      rowRes <- row env f value
      let
        (columns, values) =
          case rowRes of
            (fields, es) ->
              ( fields
              , es
              )
      pure $
        InsertInto
        table
        columns
        values
    Syntax.Bind q' _ rest -> do
      q'' <- expr env f q'
      query
        env
        (unvar (\() -> q'') f)
        (Syntax.fromScope2 rest)
    Syntax.Return q' -> query env f q'
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
                 Row vs ->
                   case fmap snd vs ^? _Cons of
                     Nothing -> Values Null []
                     Just (vv, vvs) -> Values vv vvs
                 _ -> Values yield' []
          )
          value'
          (Just n)
          m_cond'
    _ -> Left $ ExpectedQuery $ show <$> e

projectFieldNames :: Expr -> Check.ColumnInfo -> Expr
projectFieldNames value columnInfo =
  case columnInfo of
    Check.Name n _ -> Project value n
    Check.Names ns -> Row $ (fmap.fmap) (projectFieldNames value) ns

expr ::
  Show a =>
  Check.DeclEnv ->
  (a -> Expr) ->
  Syntax.Expr Info a ->
  Either CompileError Expr
expr env f e =
  case e of
    Syntax.Info _ a -> expr env f a
    Syntax.Return value -> expr env f value
    Syntax.Update{} -> error "SQL.expr env update"
    Syntax.Extend{} -> error "SQL.expr env extend"
    Syntax.Some{} -> error "SQL.expr env some"
    Syntax.None -> error "SQL.expr env some"
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
                         case Vector.find ((field ==) . fst) (Check._tiColumnInfo tableInfo) of
                           Nothing -> error $ "SQL.expr project - columnInfo not found"
                           Just (_, columnInfo) ->
                             pure $ projectFieldNames value' columnInfo
                  Check.Column ->
                    case value' of
                      Row vs ->
                        case Vector.find ((field ==) . fst) vs of
                          Nothing ->
                            error $ "SQL.expr project - invalid column projection (missing entry in row)"
                          Just (_, e') -> pure e'
                      _ -> error $ "SQL.expr project - invalid column projection"
                  _ -> error $ "SQL.expr project - invalid origin " <> show valueOrigin
    Syntax.Var n -> pure $ f n
    Syntax.Record fields ->
      Row <$>
      (traverse.traverse)
        (expr env f)
        (Vector.filter ((\case; Syntax.None -> False; _ -> True) . snd) fields)
    Syntax.Int n -> pure $ Int n
    Syntax.Bool b -> pure $ Bool b
    Syntax.IfThenElse a b c -> error "SQL.expr env ifthenelse" a b c
    Syntax.Many values -> List <$> traverse (expr env f) values
    Syntax.AND a b -> And <$> expr env f a <*> expr env f b
    Syntax.OR a b -> Or <$> expr env f a <*> expr env f b
    Syntax.EQ a b -> Eq <$> expr env f a <*> expr env f b
    Syntax.NOT a -> Not <$> expr env f a
    _ -> Subquery <$> query env f e

compileType :: Type a -> ByteString
compileType ty =
  case ty of
    Syntax.TRecord{} -> error "SQL.compileType: found TRecord"
    Syntax.TUnit{} -> error "SQL.compileType: found TUnit"
    Syntax.TBool{} -> "BOOLEAN"
    Syntax.TMany{} -> error "SQL.compileType: found TMany"
    Syntax.TQuery{} -> error "SQL.compileType: found TMany"
    Syntax.TOptional{} -> error "SQL.compileType: found TOptional"
    Syntax.TName{} -> error "SQL.compileType: found TOptional"
    Syntax.TInt{} -> "INTEGER"

compileTable :: Text -> Check.TableInfo -> SQL.Table
compileTable tableName tableInfo =
  let
    (_, colInfos, constraints) = gatherConstraints $ Check._tiItems tableInfo
  in
    SQL.Table
      (encodeUtf8 tableName)
      (fmap
          (\(n, ty) ->
            SQL.Column
            { SQL.name = encodeUtf8 n
            , SQL.type_ = compileType ty
            , SQL.notNull = True
            , SQL.autoIncrement =
                maybe False ((Syntax.AutoIncrement `elem`) . snd) $
                Map.lookup n colInfos
            }
          ) .
          Check.flattenColumnInfo . snd =<<
        Check._tiColumnInfo tableInfo
      )
      ((\(c, args) ->
          SQL.Constraint'other . SQL.Other (encodeUtf8 c) $
          encodeUtf8 <$> args
        ) <$>
        Vector.fromList constraints
      )
  where
    gatherConstraints ::
      Vector (Syntax.TableItem Check.TypeInfo) ->
      ( [Text]
      , Map
          Text -- column name
          ( Type Check.TypeInfo -- column type
          , [Syntax.Constraint] -- unary constraints
          )
      , [(Text, Vector Text)] -- higher arity constraints
      )
    gatherConstraints =
      foldl'
        (\(cols, info, constrs) i ->
           case i of
             Syntax.Field fieldName ty ->
               ( cols ++ [fieldName]
               , Map.insert fieldName (ty, []) info
               , constrs
               )
             Syntax.Constraint constr args
               | Vector.length args == 1 ->
                 ( cols
                 , Map.adjust
                     (\(ty, cs) -> (ty, cs ++ [ constr ]))
                     (args Vector.! 0)
                     info
                 , constrs
                 )
               | otherwise ->
                 ( cols
                 , info
                 , constrs ++
                   case constr of
                     Syntax.Other n -> [(n, args)]
                     Syntax.PrimaryKey -> [("PRIMARY KEY", args)]
                     Syntax.AutoIncrement -> []
                 )
        )
        ([], Map.empty, [])
