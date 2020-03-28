{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language ViewPatterns #-}
module Quill.Syntax
  ( Type(..)
  , Language(..)
  , Query(..)
  , Expr(..)
  , ShowExpr(..)
  , recordPunned
  , Decl(..)
  , TableItem(..)
  , bisubstExpr
  , bisubstQuery
  )
where

import Prelude hiding (Ordering(..))

import Bound (Scope(..))
import Bound.Scope (bitraverseScope)
import Bound.Var (Var(..), unvar)
import Control.Monad (ap)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..), bimapDefault, bifoldMapDefault)
import Data.Functor.Classes (Eq2(..), Eq1(..), eq1)
import Data.Text (Text)
import Data.Traversable (fmapDefault, foldMapDefault)
import Data.Vector (Vector)
import Data.Void (Void)

data Language
  = SQL2003
  | Postgresql
  deriving (Eq, Show)

data Type
  = TRecord (Vector (Text, Type))
  | TUnit
  | TBool
  | TMany Type
  | TQuery Type
  | TOptional Type
  | TName Text
  | TInt
  deriving (Eq, Ord, Show)

data TableItem
  = Field Text Type
  | Constraint Text (Vector Text)

data Query a b
  = SelectFrom Text
  | InsertInto (Expr b a) Text
  | InsertIntoReturning (Expr b a) Text
  | Bind (Query a b) Text (Scope () (Query a) b)
  | Return (Expr b a)
  | QVar b
  | QName Text
instance Eq2 Query where
  liftEq2 f g a b =
    case a of
      SelectFrom table ->
        case b of
          SelectFrom table' -> table == table'
          _ -> False
      InsertInto value table ->
        case b of
          InsertInto value' table ->
            liftEq2 g f value value' &&
            table == table'
          _ -> False
      InsertIntoReturning value table ->
        case b of
          InsertIntoReturning value' table ->
            liftEq2 g f value value' &&
            table == table'
          _ -> False
      Bind query n (Bound.unscope -> body) ->
        case b of
          Bind query' n' (Bound.unscope -> body') ->
            liftEq2 f g query query' &&
            liftEq2 f (liftEq (liftEq2 f g)) body body'
          _ -> False
      Return value ->
        case b of
          Return value' -> liftEq2 g f value value'
          _ -> False
      QVar x ->
        case b of
          QVar x' -> g x x'
          _ -> False
      QName n ->
        case b of
          QName n' -> n == n'
          _ -> False
instance Eq a => Eq1 (Query a) where; liftEq = liftEq2 (==)
instance (Eq a, Eq b) => Eq (Query a b) where; (==) = eq1
instance Functor (Query a) where; fmap = fmapDefault
instance Foldable (Query a) where; foldMap = foldMapDefault
instance Traversable (Query a) where; traverse = bitraverse pure
instance Bifunctor Query where; bimap = bimapDefault
instance Bifoldable Query where; bifoldMap = bifoldMapDefault
instance Bitraversable Query where
  bitraverse f g query =
    case query of
      SelectFrom a -> pure $ SelectFrom a
      InsertInto a b -> (\a' -> InsertInto a' b) <$> bitraverse g f a
      InsertIntoReturning a b -> (\a' -> InsertIntoReturning a' b) <$> bitraverse g f a
      Bind a b c -> (\a' c' -> Bind a' b c') <$> bitraverse f g a <*> bitraverseScope f g c
      Return a -> Return <$> bitraverse g f a
      QVar a -> QVar <$> g a
      QName a -> pure $ QName a
instance Applicative (Query a) where; pure = return; (<*>) = ap
instance Monad (Query a) where
  return = QVar
  (>>=) = flip (bisubstQuery pure)

bisubstScopeQuery :: (a -> Expr d c) -> (b -> Query c d) -> Scope () (Query a) b -> Scope () (Query c) d
bisubstScopeQuery f g =
  Scope .
  bisubstQuery
    (first (F . QVar) . f)
    (unvar (QVar . B) (bisubstQuery (first (F . QVar) . f) (QVar . F . g))) .
  unscope

bisubstQuery :: (a -> Expr d c) -> (b -> Query c d) -> Query a b -> Query c d
bisubstQuery f g query =
    case query of
      QName a -> QName a
      QVar a -> g a
      SelectFrom n -> SelectFrom n
      InsertInto b c -> InsertInto (bisubstExpr g f b) c
      InsertIntoReturning b c -> InsertIntoReturning (bisubstExpr g f b) c
      Bind b c d ->
        Bind (bisubstQuery f g b) c (bisubstScopeQuery f g d)
      Return b -> Return (bisubstExpr g f b)

data Expr a b
  = For
      Text
      (Expr a b) -- values
      (Maybe (Scope () (Expr a) b)) -- predicate
      (Scope () (Expr a) b) -- yield
  | Name Text
  | Var b

  | Record (Vector (Text, Expr a b))
  | Project (Expr a b) Text
  | Extend Text (Expr a b) (Expr a b)
  | Update Text (Text, Scope () (Expr a) b) (Expr a b)

  | Int Int
  | Bool Bool
  | Many (Vector (Expr a b))

  | Some (Expr a b)
  | None
  | FoldOptional (Expr a b) (Text, Scope () (Expr a) b) (Expr a b)

  | AND (Expr a b) (Expr a b)
  | OR (Expr a b) (Expr a b)
  | EQ (Expr a b) (Expr a b)
  | NOT (Expr a b)

  | Embed (Query b a)
instance Eq2 Expr where
  liftEq2 f g a b =
    case a of
      For name value (fmap Bound.unscope -> m_cond) (Bound.unscope -> body) ->
        case b of
          For name' value' (fmap Bound.unscope -> m_cond') (Bound.unscope -> body') ->
            name == name' &&
            liftEq2 f g value value' &&
            liftEq (liftEq2 f (liftEq (liftEq2 f g))) m_cond m_cond' &&
           n == n' &&
instance Eq a => Eq1 (Query a) where; liftEq = liftEq2 (==)
instance (Eq a, Eq b) => Eq (Query a b) where; (==) = eq1
instance Functor (Query a) where; fmap = fmapDefault
instance Foldable (Query a) where; foldMap = foldMapDefault
instance Traversable (Query a) where; traverse = bitraverse pure
instance Bifunctor Query where; bimap = bimapDefault
instance Bifoldable Query where; bifoldMap = bifoldMapDefault
instance Bitraversable Query where
  bitraverse f g query =
    case query of
      SelectFrom a -> pure $ SelectFrom a
      InsertInto a b -> (\a' -> InsertInto a' b) <$> bitraverse g f a
      InsertIntoReturning a b -> (\a' -> InsertIntoReturning a' b) <$> bitraverse g f a
      Bind a b c -> (\a' c' -> Bind a' b c') <$> bitraverse f g a <*> bitraverseScope f g c
      Return a -> Return <$> bitraverse g f a
      QVar a -> QVar <$> g a
      QName a -> pure $ QName a
instance Applicative (Query a) where; pure = return; (<*>) = ap
instance Monad (Query a) where
  return = QVar
  (>>=) = flip (bisubstQuery pure)

bisubstScopeQuery :: (a -> Expr d c) -> (b -> Query c d) -> Scope () (Query a) b -> Scope () (Query c) d
bisubstScopeQuery f g =
  Scope .
  bisubstQuery
    (first (F . QVar) . f)
    (unvar (QVar . B) (bisubstQuery (first (F . QVar) . f) (QVar . F . g))) .
  unscope

bisubstQuery :: (a -> Expr d c) -> (b -> Query c d) -> Query a b -> Query c d
bisubstQuery f g query =
    case query of
      QName a -> QName a
      QVar a -> g a
      SelectFrom n -> SelectFrom n
      InsertInto b c -> InsertInto (bisubstExpr g f b) c
      InsertIntoReturning b c -> InsertIntoReturning (bisubstExpr g f b) c
      Bind b c d ->
        Bind (bisubstQuery f g b) c (bisubstScopeQuery f g d)
      Return b -> Return (bisubstExpr g f b)

data Expr a b
  = For
      Text
      (Expr a b) -- values
      (Maybe (Scope () (Expr a) b)) -- predicate
      (Scope () (Expr a) b) -- yield
  | Name Text
  | Var b

  | Record (Vector (Text, Expr a b))
  | Project (Expr a b) Text
  | Extend Text (Expr a b) (Expr a b)
  | Update Text (Text, Scope () (Expr a) b) (Expr a b)

  | Int Int
  | Bool Bool
  | Many (Vector (Expr a b))

  | Some (Expr a b)
  | None
  | FoldOptional (Expr a b) (Text, Scope () (Expr a) b) (Expr a b)

  | AND (Expr a b) (Expr a b)
  | OR (Expr a b) (Expr a b)
  | EQ (Expr a b) (Expr a b)
  | NOT (Expr a b)

  | Embed (Query b a)
instance Eq2 Expr where
  liftEq2 f g a b =
    case a of
      For name value (fmap Bound.unscope -> m_cond) (Bound.unscope -> body) ->
        case b of
          For name' value' (fmap Bound.unscope -> m_cond') (Bound.unscope -> body') ->
            name == name' &&
            liftEq2 f g value value' &&
            liftEq (liftEq2 f (liftEq (liftEq2 f g))) m_cond m_cond' &&
            liftEq2 f (liftEq (liftEq2 f g)) body body'
          _ -> False
      Name n ->
        case b of
          Name n' -> n == n'
          _ -> False
      Var n ->
        case b of
          Var n' -> g n n'
          _ -> False
      Record fields ->
        case b of
          Record fields' -> liftEq (liftEq (liftEq2 f g)) fields fields'
          _ -> False
      Project record field ->
        case b of
          Project record' field' ->
            liftEq2 f g record record' &&
            field == field'
          _ -> False
      Extend name value record ->
        case b of
          Extend name' value' record' ->
            name == name' &&
            liftEq2 f g value value' &&
            liftEq2 f g record record'
          _ -> False
      Update field (name, Bound.unscope -> func) record ->
        case b of
          Update field' (name', Bound.unscope -> func') record' ->
            field == field' &&
            name == name' &&
            liftEq2 f (liftEq (liftEq2 f g)) func func' &&
            liftEq2 f g record record'
          _ -> False
      Int n ->
        case b of
          Int n' -> n == n'
          _ -> False
      Bool x ->
        case b of
          Bool x' -> x == x'
          _ -> False
      Many values ->
        case b of
          Many values' -> liftEq (liftEq2 f g) values values'
          _ -> False
      Some value ->
        case b of
          Some value' -> liftEq2 f g value value'
          _ -> False
      None ->
        case b of
          None -> True
          _ -> False
      FoldOptional z (n, Bound.unscope -> func) value ->
        case b of
          FoldOptional z' (n', Bound.unscope -> func') value' ->
            liftEq2 f g z z' &&
            n == n' &&
            liftEq2 f (liftEq (liftEq2 f g)) func func' &&
            liftEq2 f g value value'
          _ -> False
      AND l r ->
        case b of
          AND l' r' ->
            liftEq2 f g l l' &&
            liftEq2 f g r r'
          _ -> False
      OR l r ->
        case b of
          OR l' r' ->
            liftEq2 f g l l' &&
            liftEq2 f g r r'
          _ -> False
      EQ l r ->
        case b of
          EQ l' r' ->
            liftEq2 f g l l' &&
            liftEq2 f g r r'
          _ -> False
      NOT value ->
        case b of
          NOT value' -> liftEq2 f g value value'
          _ -> False
      Embed value ->
        case b of
          Embed value' -> liftEq2 g f value value'
          _ -> False
instance Eq a => Eq1 (Expr a) where; liftEq = liftEq2 (==)
instance (Eq a, Eq b) => Eq (Expr a b) where; (==) = eq1
newtype ShowExpr = ShowExpr (Expr Text Text) deriving Eq
instance Show ShowExpr where; show _ = "<expr>"
instance Functor (Expr a) where; fmap = fmapDefault
instance Foldable (Expr a) where; foldMap = foldMapDefault
instance Traversable (Expr a) where; traverse = bitraverse pure
instance Bifunctor Expr where; bimap = bimapDefault
instance Bifoldable Expr where; bifoldMap = bifoldMapDefault
instance Bitraversable Expr where
  bitraverse f g e =
    case e of
      For a b c d ->
        For a <$>
        bitraverse f g b <*>
        traverse (bitraverseScope f g) c <*>
        bitraverseScope f g d
      Name a -> pure $ Name a
      Var b -> Var <$> g b
      Record a -> Record <$> (traverse.traverse) (bitraverse f g) a
      Project a b -> (\a' -> Project a' b) <$> bitraverse f g a
      Extend a b c -> Extend a <$> bitraverse f g b <*> bitraverse f g c
      Update a (n, b) c -> (\b' -> Update a (n, b')) <$> bitraverseScope f g b <*> bitraverse f g c
      Int a -> pure $ Int a
      Bool a -> pure $ Bool a
      Many a -> Many <$> traverse (bitraverse f g) a
      Some a -> Some <$> bitraverse f g a
      None -> pure None
      FoldOptional a (n, b) c ->
        (\a' b' -> FoldOptional a' (n, b')) <$>
        bitraverse f g a <*>
        bitraverseScope f g b <*>
        bitraverse f g c
      AND a b -> AND <$> bitraverse f g a <*> bitraverse f g b
      OR a b -> OR <$> bitraverse f g a <*> bitraverse f g b
      EQ a b -> EQ <$> bitraverse f g a <*> bitraverse f g b
      NOT a -> NOT <$> bitraverse f g a
      Embed a -> Embed <$> bitraverse g f a
instance Applicative (Expr a) where; pure = return; (<*>) = ap
instance Monad (Expr a) where
  return = Var
  (>>=) = flip (bisubstExpr pure)

bisubstScopeExpr :: (a -> Query d c) -> (b -> Expr c d) -> Scope () (Expr a) b -> Scope () (Expr c) d
bisubstScopeExpr f g =
  Scope .
  bisubstExpr
    (first (F . Var) . f)
    (unvar (Var . B) (bisubstExpr (first (F . Var) . f) (Var . F . g))) .
  unscope

bisubstExpr :: (a -> Query d c) -> (b -> Expr c d) -> Expr a b -> Expr c d
bisubstExpr f g e =
  case e of
    For n value cond yield ->
      For n (bisubstExpr f g value) (bisubstScopeExpr f g <$> cond) (bisubstScopeExpr f g yield)
    Name n -> Name n
    Var n -> g n

    Record fields -> Record $ (fmap.fmap) (bisubstExpr f g) fields
    Project value field -> Project (bisubstExpr f g value) field
    Extend field value rest -> Extend field (bisubstExpr f g value) (bisubstExpr f g rest)
    Update field (n, func) rest ->
      Update
        field
        (n, bisubstScopeExpr f g func)
        (bisubstExpr f g rest)

    Int n -> Int n
    Bool b -> Bool b
    Many values -> Many $ bisubstExpr f g <$> values

    Some a -> Some (bisubstExpr f g a)
    None -> None
    FoldOptional a (n, b) c ->
      FoldOptional (bisubstExpr f g a) (n, bisubstScopeExpr f g b) (bisubstExpr f g c)

    AND a b -> AND (bisubstExpr f g a) (bisubstExpr f g b)
    OR a b -> OR (bisubstExpr f g a) (bisubstExpr f g b)
    EQ a b -> EQ (bisubstExpr f g a) (bisubstExpr f g b)
    NOT a -> NOT (bisubstExpr f g a)

    Embed a -> Embed (bisubstQuery g f a)

recordPunned :: (b -> Text) -> Vector b -> Expr a b
recordPunned f fields = Record $ (\field -> (f field, Var field)) <$> fields

data Decl
  = Table Text (Vector TableItem)
  | Type Text Type
  | Query
      Text
      (Vector (Text, Type))
      Type
      (Query (Var Int Void) Void)
  | Function
      Text
      (Vector (Text, Type))
      Type
      (Scope Int (Expr Void) Void)
