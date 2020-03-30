{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Quill.Normalise
  ( Value(..), toExpr, fromExpr
  , normaliseExpr
  )
where

import qualified Bound
import Data.Text as Text
import Data.Vector as Vector
import Quill.Syntax (Expr)
import qualified Quill.Syntax as Syntax

data Value
  = Record (Vector (Text, Value))
  | Int Int
  | Bool Bool
  | Many (Vector Value)
  | Some Value
  | None
  deriving Eq

toExpr :: Value -> Expr t a
toExpr value =
  case value of
    Record fields -> Syntax.Record $ (fmap.fmap) toExpr fields
    Int n -> Syntax.Int n
    Bool b -> Syntax.Bool b
    Many values -> Syntax.Many $ toExpr <$> values
    None -> Syntax.None
    Some a -> Syntax.Some (toExpr a)

fromExpr :: Expr t a -> Maybe Value
fromExpr value =
  case value of
    Syntax.Record fields -> Record <$> (traverse.traverse) fromExpr fields
    Syntax.Int n -> Just $ Int n
    Syntax.Bool b -> Just $ Bool b
    Syntax.Many values -> Many <$> traverse fromExpr values
    Syntax.None -> Just None
    Syntax.Some a -> Some <$> fromExpr a
    _ -> Nothing

structuralEq :: Expr t a -> Expr t a -> Maybe Bool
structuralEq l r = (==) <$> fromExpr l <*> fromExpr r

-- kleisli composition in Query monad
composeQuery ::
  Bound.Scope () (Expr t) b ->
  Bound.Scope () (Expr t) b ->
  Bound.Scope () (Expr t) b
composeQuery f (Bound.fromScope -> g) =
  Bound.toScope $ Syntax.Bind g "__temp" (Syntax.Scope2 $ Bound.F <$> f)

normaliseExpr :: Expr t a -> Expr t a
normaliseExpr e =
  case e of
    Syntax.Info t a -> Syntax.Info t $ normaliseExpr a
    Syntax.Var a -> Syntax.Var a
    Syntax.SelectFrom{} -> e
    Syntax.InsertInto value table -> Syntax.InsertInto (normaliseExpr value) table
    Syntax.InsertIntoReturning value table -> Syntax.InsertIntoReturning (normaliseExpr value) table
    Syntax.Bind m n body ->
      Syntax.underInfo' (normaliseExpr m) $ \rewrap x ->
      case x of
        Syntax.Return value ->
          normaliseExpr $ Bound.instantiate1 value $ Syntax.unscope2 body
        Syntax.Bind m' n' body' ->
          normaliseExpr $
          Syntax.Bind
            m'
            n'
            (Syntax.Scope2 $ composeQuery (Syntax.unscope2 body) (Syntax.unscope2 body'))
        _ -> Syntax.Bind (rewrap x) n $ Syntax.hoistScope2 normaliseExpr body
    Syntax.Return value -> Syntax.Return $ normaliseExpr value
    Syntax.For n value m_cond yield ->
      let
        m_cond' = Syntax.hoistScope2 normaliseExpr <$> m_cond
        yield' = Syntax.hoistScope2 normaliseExpr yield
      in
        Syntax.underInfo' (normaliseExpr value) $ \reWrap value' ->
        case value' of
          Syntax.For n2 value2 m_cond2 yield2 ->
            normaliseExpr $
            Syntax.For
              n2
              value2
              (case (m_cond2, m_cond') of
                 (Nothing, Nothing) -> Nothing
                 (Just cond2, Nothing) -> Just cond2
                 (Nothing, Just cond') -> Just cond'
                 (Just cond2, Just cond') ->
                   Just . Syntax.toScope2 $
                   Syntax.AND (Syntax.fromScope2 cond2) (Syntax.fromScope2 cond')
              )
              (Syntax.Scope2 $ Syntax.compose (Syntax.unscope2 yield') (Syntax.unscope2 yield2))
          Syntax.Many values ->
            case m_cond' of
              Nothing ->
                Syntax.Many $
                (\x -> normaliseExpr $ Bound.instantiate1 x $ Syntax.unscope2 yield') <$> values
              Just cond' ->
                let
                  m_conds =
                    traverse
                      (\x ->
                         Syntax.underInfo' (normaliseExpr $ Bound.instantiate1 x $ Syntax.unscope2 cond') $ \_ b' ->
                         case b' of
                           Syntax.Bool b -> Just b
                           _ -> Nothing
                      )
                      values
                in
                  case m_conds of
                    Nothing -> Syntax.For n (reWrap value') m_cond' yield'
                    Just conds ->
                      Syntax.Many $
                      Vector.mapMaybe
                        (\(b, v) ->
                           if b
                           then Just . normaliseExpr $ Bound.instantiate1 v $ Syntax.unscope2 yield'
                           else Nothing
                        )
                        (Vector.zip conds values)
          _ -> Syntax.For n (reWrap value') m_cond' yield'
    Syntax.Name _ -> e
    Syntax.Record values -> Syntax.Record $ (fmap.fmap) normaliseExpr values
    Syntax.Project value field ->
      Syntax.underInfo' (normaliseExpr value) $ \rewrap value' ->
      case value' of
        Syntax.Record values ->
          case Vector.find ((field ==) . fst) values of
            Just (_, result) -> Syntax.underInfo' result (\_ -> id)
            _ -> undefined
        _ -> Syntax.Project (rewrap value') field
    Syntax.HasField value field ->
      Syntax.underInfo' (normaliseExpr value) $ \rewrap value' ->
      case value' of
        Syntax.Record values ->
          case Vector.find ((field ==) . fst) values of
            Just{} -> Syntax.Bool True
            _ -> Syntax.Bool False
        _ -> Syntax.HasField (rewrap value') field
    Syntax.Extend field value record ->
      let
        value' = normaliseExpr value
      in
        Syntax.underInfo' (normaliseExpr record) $ \rewrap record' ->
        case record' of
          Syntax.Record values -> Syntax.Record $ Vector.cons (field, value') values
          _ -> Syntax.Extend field value' (rewrap record')
    Syntax.Update field (n, func) record ->
      let
        func' = Syntax.hoistScope2 normaliseExpr func
      in
        Syntax.underInfo' (normaliseExpr record) $ \rewrap record' ->
          case record' of
            Syntax.Record values ->
              Syntax.Record $
              (\(field', v) ->
                if field' == field
                then (field', normaliseExpr $ Bound.instantiate1 v $ Syntax.unscope2 func')
                else (field', v)
              ) <$>
              values
            _ ->
              Syntax.underInfo' (Syntax.fromScope2 func') $ \_ func'' ->
              case func'' of
                Syntax.Var (Bound.B ()) -> record'
                _ -> Syntax.Update field (n, func') (rewrap record')
    Syntax.Int{} -> e
    Syntax.Bool{} -> e
    Syntax.IfThenElse a b c ->
      let
        b' = normaliseExpr b
        c' = normaliseExpr c
      in
        Syntax.underInfo' (normaliseExpr a) $ \rewrap a' ->
        case a' of
          Syntax.Bool cond -> if cond then b' else c'
          _ -> Syntax.IfThenElse (rewrap a') b' c'
    Syntax.Many values -> Syntax.Many $ normaliseExpr <$> values
    Syntax.Some value -> Syntax.Some $ normaliseExpr value
    Syntax.None -> e
    Syntax.FoldOptional z (n, func) value ->
      let
        z' = normaliseExpr z
        func' = Syntax.hoistScope2 normaliseExpr func
      in
        Syntax.underInfo' (normaliseExpr value) $ \rewrap value' ->
        case value' of
          Syntax.None -> Syntax.underInfo' z' $ \_ -> id
          Syntax.Some a ->
            Syntax.underInfo' (normaliseExpr $ Bound.instantiate1 a $ Syntax.unscope2 func') $ \_ -> id
          _ -> Syntax.FoldOptional z' (n, func') (rewrap value')
    Syntax.AND l r ->
      Syntax.underInfo' (normaliseExpr l) $ \rewrapL l' ->
      Syntax.underInfo' (normaliseExpr r) $ \rewrapR r' ->
      case l' of
        Syntax.Bool False -> l'
        Syntax.Bool True -> r'
        _ ->
          case r' of
            Syntax.Bool False -> l'
            _ -> Syntax.AND (rewrapL l') (rewrapR r')
    Syntax.OR l r ->
      Syntax.underInfo' (normaliseExpr l) $ \rewrapL l' ->
      Syntax.underInfo' (normaliseExpr r) $ \rewrapR r' ->
      case l' of
        Syntax.Bool True -> l'
        Syntax.Bool False -> r'
        _ ->
          case r' of
            Syntax.Bool True -> r'
            _ -> Syntax.OR (rewrapL l') (rewrapR r')
    Syntax.EQ l r ->
      Syntax.underInfo' (normaliseExpr l) $ \rewrapL l' ->
      Syntax.underInfo' (normaliseExpr r) $ \rewrapR r' ->
      case structuralEq l' r' of
        Nothing -> Syntax.EQ (rewrapL l') (rewrapR r')
        Just b -> Syntax.Bool b
    Syntax.NOT value ->
      Syntax.underInfo' (normaliseExpr value) $ \rewrap value' ->
      case value' of
        Syntax.Bool b -> Syntax.Bool $ not b
        _ -> Syntax.NOT $ rewrap value'
