{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Quill.Marshall
  ( Value(..)
  , toExpr
  , Marshall(..)
  , fromValue
  , parseValue
  )
where

import Control.Applicative ((<|>), optional)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString (ByteString)
import Data.Foldable (foldrM)
import Data.Proxy (Proxy(..))
import qualified Data.Vector as Vector
import Data.Void (Void)
import Quill.Check (QueryEnv, TypeError, checkExpr, mkTypeInfo)
import Quill.Normalise (Value(..), toExpr, fromExpr)
import Quill.Syntax (Type)
import qualified Quill.Syntax as Syntax

class Marshall a where
  typeOf :: Proxy a -> Type ()
  toValue :: a -> Value
  fromValueUnchecked :: Value -> Maybe a

parseValue :: Type t -> ByteString -> Either String Value
parseValue = Atto.parseOnly . value id
  where
    value f ty =
      case ty of
        Syntax.TBool _ -> f <$> bool
        Syntax.TInt _ -> f <$> int
        Syntax.TOptional _ ty' ->
          value (Some . f) ty' <|>
          None <$ Atto.string "null"
        _ -> error $ "todo: support all types"
    bool =
      Bool True <$ Atto.string "t" <* optional (Atto.string "rue")<|>
      Bool False <$ Atto.string "f" <* optional (Atto.string "alse")
    int =
      Int <$> Atto.signed Atto.decimal

fromValue ::
  forall a t.
  Marshall a =>
  QueryEnv Void ->
  Value ->
  Either (TypeError t) a
fromValue env value = do
  let ty = typeOf (Proxy :: Proxy a)
  ty' <- mkTypeInfo env Nothing ty
  value' <- checkExpr env (toExpr value) ty'
  case fromValueUnchecked =<< fromExpr value' of
    Just a -> pure a
    Nothing -> error "impossible"

instance Marshall () where
  typeOf _ = Syntax.TRecord () mempty
  toValue () = Record mempty
  fromValueUnchecked value =
    case value of
      Record [] -> Just ()
      _ -> Nothing

instance Marshall a => Marshall [a] where
  typeOf _ = Syntax.TMany () $ typeOf (Proxy :: Proxy a)
  toValue = Many . Vector.map toValue . Vector.fromList
  fromValueUnchecked value =
    case value of
      Many as -> foldrM (\x xs -> (: xs) <$> fromValueUnchecked x) [] as
      _ -> Nothing

instance (Marshall a, Marshall b) => Marshall (a, b) where
  typeOf _ =
    Syntax.TRecord ()
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
  typeOf _ = Syntax.TBool ()
  toValue = Bool
  fromValueUnchecked value =
    case value of
      Bool b -> Just b
      _ -> Nothing

instance Marshall Int where
  typeOf _ = Syntax.TInt ()
  toValue = Int
  fromValueUnchecked value =
    case value of
      Int n -> Just n
      _ -> Nothing
