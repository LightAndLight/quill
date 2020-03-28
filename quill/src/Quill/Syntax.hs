{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Quill.Syntax
  ( Type(..)
  , Language(..)
  , Query(..)
  , Expr(..)
  , recordPunned
  , Decl(..)
  , TableItem(..)
  )
where

import Prelude hiding (Ordering(..))

import Bound.Class (Bound(..))
import Bound (Scope)
import Bound.Var (Var(..), unvar)
import Control.Monad (ap)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void (Void)

data Language
  = SQL2003
  | Postgresql
  deriving Eq

data Type
  = TRecord (Vector (Text, Type))
  | TUnit
  | TBool
  | TMany Type
  | TQuery Type
  | TOptional Type
  | TName Text
  | TInt
  deriving (Eq, Ord)

data TableItem
  = Field Text Type
  | Constraint Text (Vector Text)

data Query f a
  = SelectFrom Text
  | InsertInto (f a) Text
  | InsertIntoReturning (f a) Text
  | Bind (Query f a) Text (Query f (Var () a))
  | Return (f a)
  deriving (Functor, Foldable, Traversable)
instance Bound Query where
  a >>>= f =
    case a of
      SelectFrom n -> SelectFrom n
      InsertInto b c -> InsertInto (b >>= f) c
      InsertIntoReturning b c -> InsertIntoReturning (b >>= f) c
      Bind b c d ->
        Bind (b >>>= f) c (d >>>= unvar (pure . B) (fmap F . f))
      Return b -> Return (b >>= f)

data Expr a
  = For
      Text
      (Expr a) -- values
      (Maybe (Scope () Expr a)) -- predicate
      (Scope () Expr a) -- yield
  | Name Text
  | Project (Expr a) Text
  | Var a
  | Record (Vector (Text, Expr a))
  | Int Int
  | Bool Bool
  | Many (Vector (Expr a))

  | AND (Expr a) (Expr a)
  | OR (Expr a) (Expr a)
  | EQ (Expr a) (Expr a)
  | NOT (Expr a)
  deriving (Functor, Foldable, Traversable)
instance Applicative Expr where; pure = return; (<*>) = ap
instance Monad Expr where
  return = Var
  m >>= f =
    case m of
      For n value cond yield ->
        For n (value >>= f) (fmap (>>>= f) cond) (yield >>>= f)
      Name n -> Name n
      Project value field -> Project (value >>= f) field
      Var n -> f n
      Record fields -> Record $ (fmap.fmap) (>>= f) fields
      Int n -> Int n
      Bool b -> Bool b
      Many values -> Many $ (>>= f) <$> values
      AND a b -> AND (a >>= f) (b >>= f)
      OR a b -> OR (a >>= f) (b >>= f)
      EQ a b -> EQ (a >>= f) (b >>= f)
      NOT a -> NOT (a >>= f)

recordPunned :: (a -> Text) -> Vector a -> Expr a
recordPunned f fields = Record $ (\field -> (f field, Var field)) <$> fields

data Decl
  = Table Text (Vector TableItem)
  | Type Text Type
  | Query
      Text
      (Vector (Text, Type))
      Type
      (Query Expr (Var Int Void))
  | Function
      Text
      (Vector (Text, Type))
      Type
      (Scope Int Expr Void)
