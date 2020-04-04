{-# language LambdaCase #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language RankNTypes #-}
module Quill.Parser
  ( Parser, eof
  , expr
  , query
  , type_
  , decl
  , decls
  , language
  , parseString
  , parseFile
  )
where

import Prelude hiding (Ordering(..))

import qualified Bound
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Vector as Vector
import Quill.Syntax (Constraint(..), Decl(..), Expr(..), Language(..), TableItem(..), Type(..))
import qualified Quill.Syntax as Syntax
import Text.Trifecta hiding (parseString, eof)
import qualified Text.Trifecta as Trifecta
import qualified Text.Parser.Token.Highlight as Highlight

eof :: CharParsing m => m ()
eof = many space *> Trifecta.eof

varStyle :: CharParsing m => IdentifierStyle m
varStyle =
  IdentifierStyle
  { _styleName = "variable"
  , _styleStart = lower <|> char '_'
  , _styleLetter = alphaNum <|> char '_'
  , _styleReserved =
    [ "for", "in", "where", "yield", "select", "from", "insert", "into", "returning"
    , "table", "type", "query", "fn", "return"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

ctorStyle :: CharParsing m => IdentifierStyle m
ctorStyle =
  IdentifierStyle
  { _styleName = "constructor"
  , _styleStart = upper <|> char '_'
  , _styleLetter = alphaNum <|> char '_'
  , _styleReserved = []
  , _styleHighlight = Highlight.Constructor
  , _styleReservedHighlight = Highlight.ReservedConstructor
  }

reserved :: (Monad m, TokenParsing m) => Text -> m ()
reserved = reserveText varStyle

variable :: (Monad m, TokenParsing m) => m Text
variable = ident varStyle

constructor :: (Monad m, TokenParsing m) => m Text
constructor = ident ctorStyle

atomExpr :: (Monad m, TokenParsing m) => (Text -> Maybe a) -> m (Expr () a)
atomExpr var = hasField
  where
    hasField =
      (\e m_field ->
         case m_field of
           Nothing -> e
           Just field -> HasField e field
      ) <$>
      project <*>
      optional (symbolic '?' *> variable)

    project =
      foldl Project <$>
      atom <*>
      many (symbolic '.' *> variable)

    atom =
      (\v -> maybe (Name v) Var $ var v) <$> variable <|>
      Int . read <$> token (some digit) <|>
      (\c ->
         case c of
           "True" -> Bool True
           "False" -> Bool False
           "None" -> None
           _ -> fail $ "unknown constructor " <> show c
      ) <$>
        constructor <|>
      record <|>
      extend <|>
      update <|>
      list <|>
      parens (expr var)

    list =
      brackets $
      fmap (Many . Vector.fromList) $
      expr var `sepBy` comma

    extend =
      braces $
      Extend <$> variable <* symbolic ':' <*>
      expr var <* symbolic '|' <*>
      expr var

    update =
      braces $ do
        field <- variable
        _ <- symbolic '%'
        (n, func) <- do
          _ <- symbolic '\\'
          n <- variable
          _ <- symbol "->"
          value <- expr $ \n' -> if n == n' then Just (Bound.B ()) else Bound.F <$> var n'
          pure (n, Syntax.toScope2 value)
        _ <- symbolic '|'
        value <- expr var
        pure $ Update field (n, func) value

    record =
      braces $
      fmap (Record . Vector.fromList) $
      ((,) <$> variable <* symbolic ':' <*> expr var) `sepBy` comma

expr :: (Monad m, TokenParsing m) => (Text -> Maybe a) -> m (Expr () a)
expr var =
  compound <|>
  and_
  where
    compound =
      for <|>
      ifThenElse

    and_ = foldl AND <$> or_ <*> many (symbol "&&" *> or_)

    or_ = foldl OR <$> eq <*> many (symbol "||" *> eq)

    eq = (\a -> maybe a (EQ a)) <$> app <*> optional (symbol "==" *> app)

    app = do
      f <- atomExpr var
      case f of
        Name "Some" -> Some <$> atomExpr var
        Name "foldOptional" -> do
          z <- atomExpr var
          (n, func) <-
            parens $ do
              _ <- symbolic '\\'
              n <- variable
              _ <- symbol "->"
              value <- expr $ \n' -> if n == n' then Just (Bound.B ()) else Bound.F <$> var n'
              pure (n, Syntax.toScope2 value)
          value <- atomExpr var
          pure $ FoldOptional z (n, func) value
        Name "not" -> NOT <$> atomExpr var
        _ -> pure f

    ifThenElse =
      IfThenElse <$ reserved "if" <*>
      expr var <* reserved "then" <*>
      expr var <* reserved "else" <*>
      expr var

    for = do
      reserved "for"
      n <- variable
      reserved "in"
      value <- atomExpr var
      let var' = \n' -> if n == n' then Just (Bound.B ()) else Bound.F <$> var n'
      m_cond <- optional (reserved "where" *> expr var')
      reserved "yield"
      yield <- atomExpr var'
      pure $ For n value (Syntax.toScope2 <$> m_cond) (Syntax.toScope2 yield)

query :: (Monad m, TokenParsing m) => (Text -> Maybe a) -> m (Expr () a)
query var =
  bind <|>
  atom
  where
    atom =
      (\v -> maybe (Name v) Var $ var v) <$> variable <|>
      selectFrom <|>
      insertInto <|>
      return_ <|>
      parens (query var)
    selectFrom =
      SelectFrom <$ reserved "select" <* reserved "from" <*>
      constructor
    insertInto =
       (\value table m_ret ->
          case m_ret of
            Nothing -> InsertInto value table
            Just{} -> InsertIntoReturning value table
       ) <$ reserved "insert" <*>
       expr var <* reserved "into" <*>
       constructor <*>
       optional (reserved "returning")
    return_ = Return <$ reserved "return" <*> atomExpr var
    bind = do
      n <- variable
      _ <- symbol "<-"
      value <- atom
      rest <- do
        _ <- symbolic ';'
        Syntax.toScope2 <$>
          query (\n' -> if n' == n then Just (Bound.B ()) else Bound.F <$> var n')
      pure $ Bind value n rest

type_ :: (Monad m, TokenParsing m) => m (Type ())
type_ =
  app
  where
    app =
      (do
        f <- constructor
        case f of
          "Many" -> TMany () <$> atom
          "Query" -> TQuery () <$> atom
          "Optional" -> TOptional () <$> atom
          "Bool" -> pure $ TBool ()
          "Unit" -> pure $ TUnit ()
          "Int" -> pure $ TInt ()
          _ -> pure $ TName () f
      ) <|>
      atom

    atom =
      TBool () <$ symbol "Bool" <|>
      TUnit () <$ symbol "Unit" <|>
      TInt () <$ symbol "Int" <|>
      record <|>
      parens type_

    record =
      braces $
      fmap (TRecord () . Vector.fromList) $
      ((,) <$> variable <* symbolic ':' <*> type_) `sepBy` comma

decl :: (Monad m, TokenParsing m) => m (Decl () ())
decl =
  table <|>
  typeDecl <|>
  queryDecl <|>
  fnDecl
  where
    constraint =
      (\case
          "PK" -> PrimaryKey
          "AUTO_INCREMENT" -> AutoIncrement
          ctor -> Other ctor
      ) <$>
      constructor
    tableItem =
      Field <$> variable <* symbolic ':' <*> type_ <|>
      Constraint <$> constraint <*> parens (fmap Vector.fromList $ variable `sepBy1` comma)
    table =
      Table <$ reserved "table" <*>
      constructor <*>
      braces (fmap Vector.fromList $ tableItem `sepBy1` comma)
    typeDecl =
      Type <$ reserved "type" <*>
      constructor <* symbolic '=' <*>
      type_ <* symbolic ';'
    queryDecl = do
      reserved "query"
      name <- variable
      args <-
        parens . fmap Vector.fromList $
        ((,) <$> variable <* symbolic ':' <*> type_) `sepBy` comma
      _ <- symbol "->"
      retTy <- type_
      body <-
        braces $
        query
          (\n' -> fmap Bound.B . Vector.findIndex ((n' ==) . fst) $ args)
      pure $ Query name args retTy (Bound.toScope body)
    fnDecl = do
      reserved "fn"
      name <- variable
      args <-
        parens . fmap Vector.fromList $
        ((,) <$> variable <* symbolic ':' <*> type_) `sepBy` comma
      _ <- symbol "->"
      retTy <- type_
      body <-
        braces . fmap Bound.toScope $
        expr
          (\n' -> fmap Bound.B . Vector.findIndex ((n' ==) . fst) $ args)
      pure $ Function name args retTy body

decls :: (Monad m, TokenParsing m) => m [Decl () ()]
decls = some decl <* eof

language :: (Monad m, TokenParsing m) => m (Maybe Language)
language =
  symbol "#language" *> (Just Postgresql <$ symbol "postgresql") <|>
  pure Nothing

parseString :: Parser a -> String -> Either String a
parseString m str =
  case Trifecta.parseString m mempty str of
    Failure e -> Left . show $ _errDoc e
    Success a -> pure a

parseFile :: Parser a -> FilePath -> IO (Either String a)
parseFile m path =
  (\res ->
     case res of
       Failure e -> Left . show $ _errDoc e
       Success a -> pure a
  ) <$>
  Trifecta.parseFromFileEx m path
