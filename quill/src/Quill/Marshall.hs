{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Quill.Marshall
  ( Value(..)
  , toExpr
  , Marshall(..)
  , fromValue
  )
where

import Data.Foldable (foldrM)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Quill.Check (QueryEnv, TypeError, checkExpr)
import Quill.Syntax (Expr, Type)
import qualified Quill.Syntax as Syntax

data Value
  = Record (Vector (Text, Value))
  | Int Int
  | Bool Bool
  | Many (Vector Value)
  | Some Value
  | None

toExpr :: Value -> Expr Void Void
toExpr value =
  case value of
    Record fields -> Syntax.Record $ (fmap.fmap) toExpr fields
    Int n -> Syntax.Int n
    Bool b -> Syntax.Bool b
    Many values -> Syntax.Many $ toExpr <$> values
    None -> Syntax.None
    Some a -> Syntax.Some (toExpr a)

fromExpr :: Expr Void Void -> Maybe Value
fromExpr value =
  case value of
    Syntax.Record fields -> Record <$> (traverse.traverse) fromExpr fields
    Syntax.Int n -> Just $ Int n
    Syntax.Bool b -> Just $ Bool b
    Syntax.Many values -> Many <$> traverse fromExpr values
    Syntax.None -> Just None
    Syntax.Some a -> Some <$> fromExpr a
    _ -> Nothing

class Marshall a where
  typeOf :: Proxy a -> Type
  toValue :: a -> Value
  fromValueUnchecked :: Value -> Maybe a

fromValue ::
  forall a.
  Marshall a =>
  QueryEnv Void Void ->
  Value ->
  Either TypeError a
fromValue env value = do
  value' <- checkExpr env (toExpr value) $ typeOf (Proxy :: Proxy a)
  case fromValueUnchecked =<< fromExpr value' of
    Just a -> pure a
    Nothing -> error "impossible"

instance Marshall () where
  typeOf _ = Syntax.TRecord mempty
  toValue () = Record mempty
  fromValueUnchecked value =
    case value of
      Record [] -> Just ()
      _ -> Nothing

instance Marshall a => Marshall [a] where
  typeOf _ = Syntax.TMany $ typeOf (Proxy :: Proxy a)
  toValue = Many . Vector.map toValue . Vector.fromList
  fromValueUnchecked value =
    case value of
      Many as -> foldrM (\x xs -> (: xs) <$> fromValueUnchecked x) [] as
      _ -> Nothing

instance (Marshall a, Marshall b) => Marshall (a, b) where
  typeOf _ =
    Syntax.TRecord
    [ ("fst", typeOf (Proxy :: Proxy a))
    , ("snd", typeOf (Proxy :: Proxy b))
    ]
  toValue (a, b) = Record [("fst", toValue a), ("snd", toValue b)]
  fromValueUnchecked value =
    case value of
      Record [("fst", a), ("snd", b)] ->
        (,) <$>
        (fromValueUnchecked a) <*>
        (fromValueUnchecked b)
      _ -> Nothing

instance Marshall Bool where
  typeOf _ = Syntax.TBool
  toValue = Bool
  fromValueUnchecked value =
    case value of
      Bool b -> Just b
      _ -> Nothing

instance Marshall Int where
  typeOf _ = Syntax.TInt
  toValue = Int
  fromValueUnchecked value =
    case value of
      Int n -> Just n
      _ -> Nothing
