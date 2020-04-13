{-# language OverloadedStrings #-}
module Quill.Parser.Migration where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Text.Trifecta hiding (parseString, eof)

import qualified Quill.Parser as Parser
import Quill.Syntax.Migration (Command(..), FieldChange(..), Migration(..))

migration :: (Monad m, TokenParsing m) => m Migration
migration =
  initial <|>
  regular
  where
    initial =
      (\name commands ->
         Migration name Nothing $ Vector.fromList commands
      ) <$ symbol "initial" <* symbol "migration" <*>
      migrationName <*>
      braces (symbol "commands" *> symbolic ':' *> brackets (command `sepBy` comma))
    regular =
      (\name (parents, commands) ->
         Migration name (Just $ Vector.fromList parents) $ Vector.fromList commands
      ) <$ symbol "migration" <*>
      migrationName <*>
      braces
        ((,) <$>
         (symbol "parents" *> symbolic ':' *> brackets (migrationName `sepBy` comma)) <*>
         (symbol "commands" *> symbolic ':' *> brackets (command `sepBy` comma))
        )

command :: (Monad m, TokenParsing m) => m Command
command =
  createTable <|>
  alterTable
  where
    createTable =
      (\name items -> CreateTable name $ Vector.fromList items) <$ symbol "create" <* symbol "table" <*>
      Parser.constructor <*>
      braces (Parser.tableItem `sepBy` comma)
    alterTable =
      (\name changes -> AlterTable name $ Vector.fromList changes) <$ symbol "alter" <* symbol "table" <*>
      Parser.constructor <*>
      braces (fieldChange `sepBy` comma)
    fieldChange =
      DropField <$ symbol "drop" <*> Parser.variable <|>
      symbol "add" *>
        ((\name args -> AddConstraint name $ Vector.fromList args) <$ Parser.reserved "constraint" <*>
           Parser.constraint <*>
           parens (Parser.variable `sepBy` comma) <|>
         AddField <$> Parser.variable <* symbolic ':' <*> Parser.type_
        )

migrationName :: (Monad m, TokenParsing m) => m Text
migrationName =
  token $
  Text.pack <$>
  between
    (char '"')
    (char '"')
    (many (noneOf "\0\\\"" <|> char '\\' *> (char '\\' <|> char '"')))