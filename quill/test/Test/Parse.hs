{-# language GADTs #-}
{-# language OverloadedLists, OverloadedStrings #-}
module Test.Parse (parseTests) where

import qualified Bound
import Data.Void (Void)
import Test.Hspec

import Quill.Parser (Parser)
import qualified Quill.Parser as Parser
import Quill.Syntax (Constraint(..), Decl(..), Expr(..), TableItem(..), Type(..))
import qualified Quill.Syntax as Syntax

data ParseTest where
  ParseTest ::
    (Eq a, Show a) =>
    { parse_input :: [String]
    , parse_parser :: Parser a
    , parse_output :: a
    } ->
    ParseTest

parseTest :: ParseTest -> IO ()
parseTest (ParseTest { parse_input = input, parse_parser = p, parse_output = output }) =
  case Parser.parseString p (unlines input) of
    Left err -> expectationFailure err
    Right a -> a `shouldBe` output

parseTests :: Spec
parseTests = do
  it "1" $
    parseTest $
    ParseTest
    { parse_input =
      [ "type AUD = { dollars: Int, cents: Int };"
      ]
    , parse_parser = Parser.decls :: Parser [Decl () ()]
    , parse_output =
      [ Type "AUD" $ TRecord () [("dollars", TInt ()), ("cents", TInt ())]
      ]
    }
  it "2" $
    parseTest $
    ParseTest
    { parse_input =
      [ "table Expenses {"
      , "  id : Int, PK(id), AUTO_INCREMENT(id),"
      , "  name : Text,"
      , "  cost : AUD"
      , "}"
      ]
    , parse_parser = Parser.decls :: Parser [Decl () ()]
    , parse_output =
      [ Table "Expenses"
        [ Field "id" $ TInt ()
        , Constraint PrimaryKey ["id"]
        , Constraint AutoIncrement ["id"]
        , Field "name" $ TName () "Text"
        , Field "cost" $ TName () "AUD"
        ]
      ]
    }
  it "3" $
    parseTest $
    ParseTest
    { parse_input =
      [ "query selectExpensesById(id: Int) -> Query (Many { id : Int, name : Text, cost : AUD }) {"
      , "  expenses <- select from Expenses;"
      , "  return ("
      , "    for expense in expenses"
      , "    where expense.id == id"
      , "    yield expense"
      , "  )"
      , "}"
      ]
    , parse_parser = Parser.decls :: Parser [Decl () ()]
    , parse_output =
      [ Query
          "selectExpensesById"
          [("id", TInt ())]
          (TQuery () $ TMany () $
            TRecord () [("id", TInt ()), ("name", TName () "Text"), ("cost", TName () "AUD")]
          )
          (Bound.toScope . Bind (SelectFrom "Expenses") "expenses" .
            Syntax.toScope2 . Return $
            For
              "expense"
              (Var $ Bound.B ())
              (Just . Syntax.toScope2 $
              Project (Var $ Bound.B ()) "id" `Syntax.EQ` Var (Bound.F $ Bound.F $ Bound.B 0)
              )
              (Syntax.toScope2 . Var $ Bound.B ())
          )
      ]
    }
  it "4" $
    parseTest $
    ParseTest
    { parse_input =
      [ "a.b.c" ]
    , parse_parser = (Parser.expr (const Nothing) <* Parser.eof) :: Parser (Expr () Void)
    , parse_output = Project (Project (Name "a") "b") "c"
    }
