{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Quill.Normalise
  ( Value(..), toExpr, fromExpr
  , normaliseExpr, normaliseQuery
  )
where

import qualified Bound
import Bound.Scope (hoistScope)
import Data.Text as Text
import Data.Vector as Vector
import Quill.Syntax (Expr, Query)
import qualified Quill.Syntax as Syntax

data Value
  = Record (Vector (Text, Value))
  | Int Int
  | Bool Bool
  | Many (Vector Value)
  | Some Value
  | None
  deriving Eq

toExpr :: Value -> Expr a b
toExpr value =
  case value of
    Record fields -> Syntax.Record $ (fmap.fmap) toExpr fields
    Int n -> Syntax.Int n
    Bool b -> Syntax.Bool b
    Many values -> Syntax.Many $ toExpr <$> values
    None -> Syntax.None
    Some a -> Syntax.Some (toExpr a)

fromExpr :: Expr a b -> Maybe Value
fromExpr value =
  case value of
    Syntax.Record fields -> Record <$> (traverse.traverse) fromExpr fields
    Syntax.Int n -> Just $ Int n
    Syntax.Bool b -> Just $ Bool b
    Syntax.Many values -> Many <$> traverse fromExpr values
    Syntax.None -> Just None
    Syntax.Some a -> Some <$> fromExpr a
    _ -> Nothing

structuralEq :: Expr a b -> Expr a b -> Maybe Bool
structuralEq l r = (==) <$> fromExpr l <*> fromExpr r

-- kleisli composition
composeQuery ::
  Bound.Scope () (Query a) b ->
  Bound.Scope () (Query a) b ->
  Bound.Scope () (Query a) b
composeQuery f (Bound.fromScope -> g) =
  Bound.toScope $ Syntax.Bind g "__temp" (Bound.F <$> f)

normaliseQuery :: Query a b -> Query a b
normaliseQuery query =
  case query of
    Syntax.SelectFrom{} -> query
    Syntax.QName{} -> query
    Syntax.QVar{} -> query
    Syntax.InsertInto value table -> Syntax.InsertInto (normaliseExpr value) table
    Syntax.InsertIntoReturning value table -> Syntax.InsertIntoReturning (normaliseExpr value) table
    Syntax.Bind m n body ->
      case normaliseQuery m of
        Syntax.Return value -> Bound.instantiate1 (Syntax.Return value) body
        Syntax.Bind m' n' body' ->
          normaliseQuery $
          Syntax.Bind m' n' (composeQuery body body')
        m' -> Syntax.Bind m' n $ hoistScope normaliseQuery body
    Syntax.Return value -> Syntax.Return $ normaliseExpr value

normaliseExpr :: Expr a b -> Expr a b
normaliseExpr e =
  case e of
    Syntax.For n value m_cond yield ->
      let
        value' = normaliseExpr value
        m_cond' = hoistScope normaliseExpr <$> m_cond
        yield' = hoistScope normaliseExpr yield
      in
        case value' of
          Syntax.Many values ->
            Syntax.Many $
            case m_cond' of
              Nothing ->
                (\x -> normaliseExpr $ Bound.instantiate1 x yield') <$> values
              Just cond' ->
                Vector.mapMaybe
                  (\x ->
                     case normaliseExpr $ Bound.instantiate1 x cond' of
                       Syntax.Bool b ->
                         if b
                         then Just . normaliseExpr $ Bound.instantiate1 x yield'
                         else Nothing
                       _ -> undefined
                  )
                  values
          _ -> Syntax.For n value' m_cond' yield'
    Syntax.Name{} -> e
    Syntax.Var{} -> e
    Syntax.Record values -> Syntax.Record $ (fmap.fmap) normaliseExpr values
    Syntax.Project value field ->
      case normaliseExpr value of
        Syntax.Record values ->
          case Vector.find ((field ==) . fst) values of
            Just (_, result) -> result
            _ -> undefined
        value' -> Syntax.Project value' field
    Syntax.Extend field value record ->
      let
        value' = normaliseExpr value
      in
        case normaliseExpr record of
          Syntax.Record values -> Syntax.Record $ Vector.cons (field, value') values
          record' -> Syntax.Extend field value' record'
    Syntax.Update field (n, func) record ->
      let
        func' = hoistScope normaliseExpr func
      in
        case normaliseExpr record of
          Syntax.Record values ->
            Syntax.Record $
            (\(field', v) ->
               if field' == field
               then (field', normaliseExpr $ Bound.instantiate1 v func')
               else (field', v)
            ) <$>
            values
          record' -> Syntax.Update field (n, func') record'
    Syntax.Int{} -> e
    Syntax.Bool{} -> e
    Syntax.Many values -> Syntax.Many $ normaliseExpr <$> values
    Syntax.Some value -> Syntax.Some $ normaliseExpr value
    Syntax.None -> e
    Syntax.FoldOptional z (n, func) value ->
      let
        z' = normaliseExpr z
        func' = hoistScope normaliseExpr func
      in
        case normaliseExpr value of
          Syntax.None -> z'
          Syntax.Some a -> normaliseExpr $ Bound.instantiate1 a func'
          value' -> Syntax.FoldOptional z' (n, func') value'
    Syntax.AND l r ->
      let
        l' = normaliseExpr l
        r' = normaliseExpr r
      in
        case l' of
          Syntax.Bool False -> Syntax.Bool False
          Syntax.Bool True -> r'
          _ ->
            case r' of
              Syntax.Bool False -> Syntax.Bool False
              _ -> Syntax.AND l' r'
    Syntax.OR l r ->
      let
        l' = normaliseExpr l
        r' = normaliseExpr r
      in
        case l' of
          Syntax.Bool True -> Syntax.Bool True
          Syntax.Bool False -> r'
          _ ->
            case r' of
              Syntax.Bool True -> Syntax.Bool True
              _ -> Syntax.OR l' r'
    Syntax.EQ l r ->
      let
        l' = normaliseExpr l
        r' = normaliseExpr r
      in
        case structuralEq l' r' of
          Nothing -> Syntax.EQ l' r'
          Just b -> Syntax.Bool b
    Syntax.NOT value ->
      case normaliseExpr value of
        Syntax.Bool b -> Syntax.Bool $ not b
        value' -> Syntax.NOT value'
    Syntax.Embed query -> Syntax.Embed $ normaliseQuery query
