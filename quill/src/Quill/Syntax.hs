{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Quill.Syntax
  ( Type(..)
  , getTypeAnn
  , prettyType
  , Language(..)
  , Expr(..)
  , underInfo
  , underInfo'
  , getAnn
  , prettyExpr
  , recordPunned
  , Decl(..)
  , prettyDecl
  , TableItem(..)
  , prettyTableItem
  , Scope2(..)
  , fromScope2
  , toScope2
  , hoistScope2
  , compose
  )
where

import Prelude hiding (Ordering(..))

import Bound ((>>>=), Scope(..))
import Bound.Scope (hoistScope)
import qualified Bound
import Bound.Var (unvar)
import Control.Monad (ap)
import Data.Bifunctor (Bifunctor(..))
import Data.Bifunctor.TH (deriveBifunctor)
import Data.Deriving (deriveEq2, deriveShow2)
import Data.Foldable (fold)
import Data.Functor.Classes (Eq1(..), Show1(..), Eq2(..), Show2(..), eq1, showsPrec1, showsUnaryWith)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

data Language
  = SQL2003
  | Postgresql
  deriving (Eq, Show)

data Type t
  = TRecord t (Vector (Text, Type t))
  | TUnit t
  | TBool t
  | TMany t (Type t)
  | TQuery t (Type t)
  | TOptional t (Type t)
  | TName t Text
  | TInt t
  deriving (Eq, Ord, Show, Functor)

getTypeAnn :: Type t -> t
getTypeAnn t =
  case t of
    TRecord a _ -> a
    TUnit a -> a
    TBool a -> a
    TMany a _ -> a
    TQuery a _ -> a
    TOptional a _ -> a
    TName a _ -> a
    TInt a -> a

data TableItem t
  = Field Text (Type t)
  | Constraint Text (Vector Text)
  deriving (Eq, Show)

newtype Scope2 x f a b = Scope2 { unscope2 :: Scope x (f a) b }
  deriving (Functor, Foldable, Traversable)
fromScope2 :: Monad (f a) => Scope2 x f a b -> f a (Bound.Var x b)
fromScope2 (Scope2 a) = Bound.fromScope a
toScope2 :: Monad (f a) => f a (Bound.Var x b) -> Scope2 x f a b
toScope2 = Scope2 . Bound.toScope
substScope2 :: Monad (f a) => Scope2 x f a b -> (b -> f a c) -> Scope2 x f a c
substScope2 (Scope2 a) f = Scope2 (a >>>= f)
hoistScope2 :: Functor (f a) => (forall y. f a y -> g a' y) -> Scope2 x f a b -> Scope2 x g a' b
hoistScope2 f (Scope2 a) = Scope2 (hoistScope f a)
instance (Eq x, Eq2 f) => Eq2 (Scope2 x f) where
  liftEq2 f g (Scope2 (Scope a)) (Scope2 (Scope b)) =
    liftEq2 f (liftEq (liftEq2 f g)) a b
instance (Show x, Show2 f) => Show2 (Scope2 x f) where
  liftShowsPrec2 s sl s' sl' p (Scope2 scope) =
    showsUnaryWith
      (liftShowsPrec2
         s
         sl
         (liftShowsPrec (liftShowsPrec2 s sl s' sl') (liftShowList2 s sl s' sl'))
         (liftShowList (liftShowsPrec2 s sl s' sl') (liftShowList2 s sl s' sl'))
      )
      "Scope2"
      p
      (unscope scope)
instance (Eq x, Eq2 f, Eq a) => Eq1 (Scope2 x f a) where; liftEq = liftEq2 (==)
instance (Show x, Show2 f, Show a) => Show1 (Scope2 x f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Bifunctor f => Bifunctor (Scope2 x f) where
  bimap f g =
    Scope2 . Scope .
    bimap f (fmap (bimap f g)) .
    unscope . unscope2

underInfo :: Expr t a -> (Expr t a -> Expr t a) -> Expr t a
underInfo e f =
  case e of
    Info t a -> Info t $ underInfo a f
    _ -> f e

underInfo' :: Expr t a -> ((Expr t a -> Expr t a) -> Expr t a -> x) -> x
underInfo' = go id
  where
    go g e f =
      case e of
        Info t a -> go (g . Info t) a f
        _ -> f g e

data Expr t a
  = Name Text
  | Var a
  | Info t (Expr t a)

  | SelectFrom Text
  | InsertInto (Expr t a) Text
  | InsertIntoReturning (Expr t a) Text
  | Bind (Expr t a) Text (Scope2 () Expr t a)
  | Return (Expr t a)

  | For
      Text
      (Expr t a) -- values
      (Maybe (Scope2 () Expr t a)) -- predicate
      (Scope2 () Expr t a) -- yield

  | Record (Vector (Text, Expr t a))
  | Project (Expr t a) Text
  | Extend Text (Expr t a) (Expr t a)
  | Update Text (Text, Scope2 () Expr t a) (Expr t a)
  | HasField (Expr t a) Text

  | Int Int

  | Bool Bool
  | IfThenElse (Expr t a) (Expr t a) (Expr t a)

  | Many (Vector (Expr t a))

  | Some (Expr t a)
  | None
  | FoldOptional (Expr t a) (Text, Scope2 () Expr t a) (Expr t a)

  | AND (Expr t a) (Expr t a)
  | OR (Expr t a) (Expr t a)
  | EQ (Expr t a) (Expr t a)
  | NOT (Expr t a)
  deriving (Functor, Foldable, Traversable)
deriveBifunctor ''Expr
deriveEq2 ''Expr
deriveShow2 ''Expr
instance Eq a => Eq1 (Expr a) where; liftEq = liftEq2 (==)
instance Show a => Show1 (Expr a) where; liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Eq t, Eq a) => Eq (Expr t a) where; (==) = eq1
instance (Show t, Show a) => Show (Expr t a) where; showsPrec = showsPrec1
instance Applicative (Expr t) where; pure = return; (<*>) = ap
instance Monad (Expr t) where
  return = Var
  m >>= f =
    case m of
      Name n -> Name n
      Var a -> f a
      Info a b -> Info a (b >>= f)

      SelectFrom table -> SelectFrom table
      InsertInto value table -> InsertInto (value >>= f) table
      InsertIntoReturning value table -> InsertIntoReturning (value >>= f) table
      Bind value n rest -> Bind (value >>= f) n (substScope2 rest f)
      Return value -> Return (value >>= f)

      For n value m_cond yield -> For n (value >>= f) ((`substScope2` f) <$> m_cond) (substScope2 yield f)

      Record values -> Record ((fmap.fmap) (>>= f) values)
      Project value field -> Project (value >>= f) field
      HasField value field -> HasField (value >>= f) field
      Extend field value rest -> Extend field (value >>= f) (rest >>= f)
      Update field (n, func) rest -> Update field (n, substScope2 func f) (rest >>= f)

      Int n -> Int n

      Bool b -> Bool b
      IfThenElse a b c -> IfThenElse (a >>= f) (b >>= f) (c >>= f)

      Many values -> Many ((>>= f) <$> values)

      Some value -> Some (value >>= f)
      None -> None
      FoldOptional z (n, func) value -> FoldOptional (z >>= f) (n, substScope2 func f) (value >>= f)

      AND l r -> AND (l >>= f) (r >>= f)
      OR l r -> OR (l >>= f) (r >>= f)
      EQ l r -> EQ (l >>= f) (r >>= f)
      NOT value -> NOT (value >>= f)

recordPunned :: (a -> Text) -> Vector a -> Expr t a
recordPunned f fields = Record $ (\field -> (f field, Var field)) <$> fields

getAnn :: Expr t a -> Maybe t
getAnn e =
  case e of
    Info a _ -> Just a
    _ -> Nothing

data Decl t t'
  = Table Text (Vector (TableItem t'))
  | Type Text (Type t')
  | Query
      Text
      (Vector (Text, Type t'))
      (Type t')
      (Scope Int (Expr t) Void)
  | Function
      Text
      (Vector (Text, Type t'))
      (Type t')
      (Scope Int (Expr t) Void)
  deriving (Eq, Show)

prettyExpr :: (a -> Doc) -> Expr t a -> Doc
prettyExpr f e =
  case e of
    Info _ a -> prettyExpr f a
    SelectFrom table ->
      Pretty.hsep
      [ Pretty.text "select"
      , Pretty.text "from"
      , Pretty.text $ Text.unpack table
      ]
    InsertInto value table ->
      Pretty.hsep
      [ Pretty.text "insert"
      , prettyExpr f value
      , Pretty.text "into"
      , Pretty.text $ Text.unpack table
      ]
    InsertIntoReturning value table ->
      Pretty.hsep
      [ Pretty.text "insert"
      , prettyExpr f value
      , Pretty.text "into"
      , Pretty.text $ Text.unpack table
      , Pretty.text "returning"
      ]
    Bind value n rest ->
      let
        n' = Pretty.text (Text.unpack n)
      in
        Pretty.vsep
        [ Pretty.hsep
          [ n'
          , Pretty.text "<-"
          , prettyExpr f value <> Pretty.char ';'
          ]
        , prettyExpr (unvar (\() -> n') f) (fromScope2 rest)
        ]
    Return a -> Pretty.text "return" <> Pretty.space <> prettyExpr f a
    For n value m_cond (fromScope2 -> yield) ->
      let
        n' = Pretty.text $ Text.unpack n
      in
        Pretty.vsep $
        [ Pretty.hsep
          [ Pretty.text "for"
          , Pretty.text $ Text.unpack n
          , Pretty.text "in"
          , prettyExpr f value
          ]
        ] <>
        (maybe
          []
          (\(fromScope2 -> cond) ->
            [ Pretty.hsep
              [ Pretty.text "where"
              , prettyExpr (unvar (\() -> n') f) cond
              ]
            ]
          )
          m_cond
        ) <>
        [ Pretty.hsep
          [ Pretty.text "yield"
          , prettyExpr (unvar (\() -> n') f) yield
          ]
        ]
    Name n -> Pretty.text $ Text.unpack n
    Var n -> f n
    Record values ->
      Pretty.braces . fold . List.intersperse (Pretty.comma <> Pretty.space) $
      foldr
        (\(field, v) ->
           (:) (Pretty.text (Text.unpack field) <> Pretty.char ':' <> Pretty.space <> prettyExpr f v)
        )
        []
        values
    HasField record field -> prettyExpr f record <> Pretty.char '?' <> Pretty.text (Text.unpack field)
    Project value field ->
      prettyExpr f value <> Pretty.dot <> Pretty.text (Text.unpack field)
    Extend field value record ->
      Pretty.braces $
      Pretty.hsep
      [ Pretty.text (Text.unpack field) <> Pretty.char ':'
      , prettyExpr f value
      , Pretty.char '|'
      , prettyExpr f record
      ]
    Update field (n, fromScope2 -> func) record ->
      let
        n' = Pretty.text (Text.unpack n)
      in
      Pretty.braces $
      Pretty.hsep
      [ Pretty.text $ Text.unpack field
      , Pretty.char '%'
      , Pretty.char '\\' <> n'
      , Pretty.text "->"
      , prettyExpr (unvar (\() -> n') f) func
      , Pretty.char '|'
      , prettyExpr f record
      ]
    Int n -> Pretty.text $ show n
    Bool b -> Pretty.text $ show b
    IfThenElse a b c ->
      Pretty.hsep
      [ Pretty.text "if"
      , prettyExpr f a
      , Pretty.text "then"
      , prettyExpr f b
      , Pretty.text "else"
      , prettyExpr f c
      ]
    Many values ->
      Pretty.brackets . fold . List.intersperse (Pretty.comma <> Pretty.space) $
      foldr ((:) . prettyExpr f) [] values
    Some value ->
      Pretty.hsep [Pretty.text "Some", prettyExpr f value]
    None -> Pretty.text "None"
    FoldOptional z (n, fromScope2 -> func) value ->
      let
        n' = Pretty.text (Text.unpack n)
      in
      Pretty.hsep
      [ Pretty.text "foldOptional"
      , prettyExpr f z
      , Pretty.parens $
        Pretty.hsep
        [ Pretty.char '\\' <> n'
        , Pretty.text "->"
        , prettyExpr (unvar (\() -> n') f) func
        ]
      , (case value of
           FoldOptional{} -> Pretty.parens
           Some{} -> Pretty.parens
           _ -> id
        ) (prettyExpr f value)
      ]
    AND l r ->
      Pretty.hsep [prettyExpr f l, Pretty.text "&&", prettyExpr f r]
    OR l r ->
      Pretty.hsep [prettyExpr f l, Pretty.text "||", prettyExpr f r]
    EQ l r ->
      Pretty.hsep [prettyExpr f l, Pretty.text "==", prettyExpr f r]
    NOT value ->
      Pretty.hsep [Pretty.text "not", prettyExpr f value]

prettyType :: Type t -> Doc
prettyType ty =
  case ty of
    TRecord _ fields ->
      Pretty.braces . (Pretty.space <>) . (<> Pretty.space) . fold .
      List.intersperse (Pretty.comma <> Pretty.space) $
      foldr
        (\(f, t) ->
           (:) $
           Pretty.hsep
           [ Pretty.text $ Text.unpack f
           , Pretty.char ':'
           , prettyType t
           ]
        )
        []
        fields
    TMany _ a ->
      Pretty.text "Many" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TQuery _ a ->
      Pretty.text "Query" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TOptional _ a ->
      Pretty.text "Optional" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TName _ n -> Pretty.text $ Text.unpack n
    TInt _ -> Pretty.text "Int"
    TUnit _ -> Pretty.text "Unit"
    TBool _ -> Pretty.text "Bool"

prettyTableItem :: TableItem t -> Doc
prettyTableItem item =
  case item of
    Field field ty -> Pretty.hsep [Pretty.text (Text.unpack field), Pretty.char ':', prettyType ty]
    Constraint name args ->
      Pretty.text (Text.unpack name) <>
      Pretty.parens (fold . List.intersperse Pretty.comma $ foldr ((:) . Pretty.text . Text.unpack) [] args)

prettyDecl :: Decl t t' -> Doc
prettyDecl d =
  case d of
    Table table items ->
      Pretty.vsep
      [ Pretty.hsep [Pretty.text "table", Pretty.text (Text.unpack table), Pretty.char '{']
      , fold . List.intersperse (Pretty.char ',' <> Pretty.line) $
        foldr ((:) . Pretty.indent 2 . prettyTableItem) [] items
      , Pretty.char '}'
      ]
    Type name value ->
      Pretty.hsep
      [ Pretty.text "type"
      , Pretty.text $ Text.unpack name
      , Pretty.char '='
      , prettyType value <> Pretty.char ';'
      ]
    Query name args retTy body ->
      Pretty.vsep
      [ Pretty.hsep
        [ Pretty.text "query"
        , Pretty.text (Text.unpack name) <>
          Pretty.parens
          (fold . List.intersperse Pretty.comma $
           foldr
             (\(f, t) ->
                (:) (Pretty.text (Text.unpack f) <> Pretty.char ':' <> Pretty.space <> prettyType t)
             )
             []
             args
          )
        , Pretty.text "->"
        , prettyType retTy
        , Pretty.char '{'
        ]
      , Pretty.indent 2 $
        prettyExpr
          (unvar (Pretty.text . Text.unpack . fst . (args Vector.!)) absurd)
          (Bound.fromScope body)
      , Pretty.char '}'
      ]
    Function name args retTy body ->
      Pretty.vsep
      [ Pretty.hsep
        [ Pretty.text "query"
        , Pretty.text (Text.unpack name) <>
          Pretty.parens
          (fold . List.intersperse Pretty.comma $
           foldr
             (\(f, t) ->
                (:) (Pretty.text (Text.unpack f) <> Pretty.char ':' <> Pretty.space <> prettyType t)
             )
             []
             args
          )
        , Pretty.text "->"
        , prettyType retTy
        , Pretty.char '{'
        ]
      , Pretty.indent 2 $
        prettyExpr
          (unvar (Pretty.text . Text.unpack . fst . (args Vector.!)) absurd)
          (Bound.fromScope body)
      , Pretty.char '}'
      ]

compose :: Scope () (Expr t) a -> Scope () (Expr t) a -> Scope () (Expr t) a
compose f (Bound.fromScope -> g) =
  Bound.toScope $
  Bound.instantiate1 g $ Bound.F <$> f
