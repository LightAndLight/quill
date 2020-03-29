{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Quill.Syntax
  ( Type(..)
  , prettyType
  , Language(..)
  , Expr(..)
  , prettyExpr
  , recordPunned
  , Decl(..)
  , prettyDecl
  , TableItem(..)
  , prettyTableItem
  )
where

import Prelude hiding (Ordering(..))

import Bound ((>>>=), Scope(..))
import qualified Bound
import Bound.Var (unvar)
import Control.Monad (ap)
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Foldable (fold)
import Data.Functor.Classes (eq1, showsPrec1)
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
  deriving (Eq, Show)

data Expr a
  = Name Text
  | Var a

  | SelectFrom Text
  | InsertInto (Expr a) Text
  | InsertIntoReturning (Expr a) Text
  | Bind (Expr a) Text (Scope () Expr a)
  | Return (Expr a)

  | For
      Text
      (Expr a) -- values
      (Maybe (Scope () Expr a)) -- predicate
      (Scope () Expr a) -- yield

  | Record (Vector (Text, Expr a))
  | Project (Expr a) Text
  | Extend Text (Expr a) (Expr a)
  | Update Text (Text, Scope () Expr a) (Expr a)
  | HasField (Expr a) Text

  | Int Int

  | Bool Bool
  | IfThenElse (Expr a) (Expr a) (Expr a)

  | Many (Vector (Expr a))

  | Some (Expr a)
  | None
  | FoldOptional (Expr a) (Text, Scope () Expr a) (Expr a)

  | AND (Expr a) (Expr a)
  | OR (Expr a) (Expr a)
  | EQ (Expr a) (Expr a)
  | NOT (Expr a)
  deriving (Functor, Foldable, Traversable)
deriveEq1 ''Expr
deriveShow1 ''Expr
instance Eq a => Eq (Expr a) where; (==) = eq1
instance Show a => Show (Expr a) where; showsPrec = showsPrec1
instance Applicative Expr where; pure = return; (<*>) = ap
instance Monad Expr where
  return = Var
  m >>= f =
    case m of
      Name n -> Name n
      Var a -> f a

      SelectFrom table -> SelectFrom table
      InsertInto value table -> InsertInto (value >>= f) table
      InsertIntoReturning value table -> InsertIntoReturning (value >>= f) table
      Bind value n rest -> Bind (value >>= f) n (rest >>>= f)
      Return value -> Return (value >>= f)

      For n value m_cond yield -> For n (value >>= f) ((>>>= f) <$> m_cond) (yield >>>= f)

      Record values -> Record ((fmap.fmap) (>>= f) values)
      Project value field -> Project (value >>= f) field
      HasField value field -> HasField (value >>= f) field
      Extend field value rest -> Extend field (value >>= f) (rest >>= f)
      Update field (n, func) rest -> Update field (n, func >>>= f) (rest >>= f)

      Int n -> Int n

      Bool b -> Bool b
      IfThenElse a b c -> IfThenElse (a >>= f) (b >>= f) (c >>= f)

      Many values -> Many ((>>= f) <$> values)

      Some value -> Some (value >>= f)
      None -> None
      FoldOptional z (n, func) value -> FoldOptional (z >>= f) (n, func >>>= f) (value >>= f)

      AND l r -> AND (l >>= f) (r >>= f)
      OR l r -> OR (l >>= f) (r >>= f)
      EQ l r -> EQ (l >>= f) (r >>= f)
      NOT value -> NOT (value >>= f)

recordPunned :: (a -> Text) -> Vector a -> Expr a
recordPunned f fields = Record $ (\field -> (f field, Var field)) <$> fields


data Decl
  = Table Text (Vector TableItem)
  | Type Text Type
  | Query
      Text
      (Vector (Text, Type))
      Type
      (Scope Int Expr Void)
  | Function
      Text
      (Vector (Text, Type))
      Type
      (Scope Int Expr Void)
  deriving (Eq, Show)

prettyExpr :: (a -> Doc) -> Expr a -> Doc
prettyExpr f e =
  case e of
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
        , prettyExpr (unvar (\() -> n') f) (Bound.fromScope rest)
        ]
    Return a -> Pretty.text "return" <> Pretty.space <> prettyExpr f a
    For n value m_cond (Bound.fromScope -> yield) ->
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
          (\(Bound.fromScope -> cond) ->
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
    Update field (n, Bound.fromScope -> func) record ->
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
    FoldOptional z (n, Bound.fromScope -> func) value ->
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

prettyType :: Type -> Doc
prettyType ty =
  case ty of
    TRecord fields ->
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
    TMany a ->
      Pretty.text "Many" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TQuery a ->
      Pretty.text "Query" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TOptional a ->
      Pretty.text "Optional" <> Pretty.space <>
      (case a of
         TQuery{} -> Pretty.parens
         TMany{} -> Pretty.parens
         TOptional{} -> Pretty.parens
         _ -> id
      ) (prettyType a)
    TName n -> Pretty.text $ Text.unpack n
    TInt -> Pretty.text "Int"
    TUnit -> Pretty.text "Unit"
    TBool -> Pretty.text "Bool"

prettyTableItem :: TableItem -> Doc
prettyTableItem item =
  case item of
    Field field ty -> Pretty.hsep [Pretty.text (Text.unpack field), Pretty.char ':', prettyType ty]
    Constraint name args ->
      Pretty.text (Text.unpack name) <>
      Pretty.parens (fold . List.intersperse Pretty.comma $ foldr ((:) . Pretty.text . Text.unpack) [] args)

prettyDecl :: Decl -> Doc
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
