{-# language BangPatterns #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}
module Quill.Query
  ( QueryEnv
  , createTable
  , load
  , query
  , decodeRecord
  , loadString
  , migrate
  )
where

import qualified Bound.Scope as Bound
import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..), Result(..))
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Lens.Indexed (itraverse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (foldlM, for_)
import qualified Data.Graph as Graph
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (Void, absurd)
import Text.Show.Deriving (makeShow)

import qualified Data.Graph.Extra as Graph (roots)
import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import qualified Quill.Check as Check
import qualified Quill.Check.Migration as Check (_meMigrations, emptyMigrationEnv, checkMigrations)
import Quill.Marshall (Value)
import qualified Quill.Marshall as Marshall
import Quill.Normalise (toExpr)
import qualified Quill.Normalise as Normalise
import qualified Quill.Parser as Parser
import qualified Quill.SQL as SQL
import qualified Quill.SQL.Migration as SQL (compileMigration)
import qualified Quill.Syntax as Syntax
import Quill.Syntax.Migration (Migration)
import qualified Quill.Syntax.Migration as Migration

data QueryException
  = ParseError String
  | CheckError (Check.DeclError () ())
  | QueryNotFound Text
  | TableNotFound Text
  | ArgumentMismatch Int Int
  | ArgumentCheckError Int (Check.TypeError Check.TypeInfo)
  | CompileError SQL.CompileError
  | TooManyRows Int64 (Syntax.Type Check.TypeInfo)
  | ColumnMismatch Int Int64 (Syntax.Type Check.TypeInfo)
  | DecodeError ByteString String
  | UnexpectedResponse Response
  deriving Typeable
$(pure [])
showQueryException :: QueryException -> String
showQueryException = $(makeShow ''QueryException)
instance Show QueryException where
  show e =
    case e of
      ParseError s -> "ParseError:\n\n" <> s
      _ -> showQueryException e
instance Exception QueryException

data QueryEnv = QueryEnv { _qeBackend :: Backend, _qeDeclEnv :: Check.DeclEnv }

loadString :: Backend -> String -> IO QueryEnv
loadString backend file = do
  let res = Parser.parseString ((,) <$> Parser.language <*> Parser.decls) file
  (lang, decls) <- either (throw . ParseError) pure res
  (_, declEnv) <-
    either
      (throw . CheckError)
      pure
      (Check.checkDecls (Check.emptyDeclEnv { Check._deLanguage = lang }) $ Vector.fromList decls)
  pure $
    QueryEnv
    { _qeBackend = backend
    , _qeDeclEnv = declEnv { Check._deLanguage = Just Syntax.Postgresql }
    }

load :: Backend -> FilePath -> IO QueryEnv
load backend path = do
  res <- Parser.parseFile ((,) <$> Parser.language <*> Parser.decls) path
  (lang, decls) <- either (throw . ParseError) pure res
  (_, declEnv) <-
    either
      (throw . CheckError)
      pure
      (Check.checkDecls (Check.emptyDeclEnv { Check._deLanguage = lang }) $ Vector.fromList decls)
  pure $
    QueryEnv
    { _qeBackend = backend
    , _qeDeclEnv = declEnv { Check._deLanguage = Just Syntax.Postgresql }
    }

decodeValue ::
  Vector ByteString ->
  Syntax.Type t ->
  IO Value
decodeValue res ty = do
  let m_val = res Vector.!? 0
  case m_val of
    Nothing ->
      error "impossible: getvalue' returned Nothing"
    Just val ->
      either (liftIO . throw . DecodeError val) pure $
      Marshall.parseValue ty val

decodeRecord ::
  Vector ByteString ->
  Vector (Text, Syntax.Type t) ->
  IO Value
decodeRecord res a = evalStateT (go a) 0
  where
    goType :: Syntax.Type t -> StateT Int64 IO Value
    goType ty =
      case ty of
        Syntax.TRecord _ fields -> go fields
        _ -> do
          col <- get
          let m_val = res Vector.!? fromIntegral col
          put $ col + 1
          case m_val of
            Nothing ->
              error "impossible: missing column"
            Just val ->
              either (liftIO . throw . DecodeError val) pure $
              Marshall.parseValue ty val

    go :: Vector (Text, Syntax.Type t) -> StateT Int64 IO Value
    go fields =
      Normalise.Record <$> traverse (\(n, t) -> (,) n <$> goType t) fields

resultResponse :: Response -> IO Result
resultResponse res =
  case res of
    Response'result result -> pure result
    _ -> throw $ UnexpectedResponse res

doneResponse :: Response -> IO ()
doneResponse res =
  case res of
    Response'done -> pure ()
    _ -> throw $ UnexpectedResponse res

resultOrDoneResponse :: Response -> IO (Maybe Result)
resultOrDoneResponse res =
  case res of
    Response'result result -> pure $ Just result
    Response'done -> pure Nothing
    _ -> throw $ UnexpectedResponse res

query :: QueryEnv -> Text -> Vector Value -> IO Value
query (QueryEnv backend env) queryName args = do
  entry <-
    maybe (throw $ QueryNotFound queryName) pure $
    Map.lookup queryName (Check._deGlobalQueries env)
  let
    argTys = Check._qeArgTys entry
    retTy = Check._qeRetTy entry
    expectedArgCount = Vector.length argTys
    actualArgCount = Vector.length args
  when (expectedArgCount /= actualArgCount) . throw $
    ArgumentMismatch expectedArgCount actualArgCount
  let env' = Check.toQueryEnv env
  args' <-
    itraverse
      (\i (v, t) ->
         either (throw . ArgumentCheckError i) (pure . fst) $
         Check.checkExpr env' (toExpr v) t
      )
      (Vector.zip args argTys)
  q <-
    either (throw . CompileError) pure .
    SQL.query env absurd .
    Normalise.normaliseExpr $
    Bound.instantiate (args' Vector.!) (Check._qeBody entry)
  let stmt = Lazy.toStrict . Builder.toLazyByteString $ SQL.compileQuery q
  res <- Backend.request backend $ Request'exec stmt
  case retTy of
    Syntax.TQuery _ (Syntax.TMany _ rowTy) -> do
      Result { rows = _, columns = numColumns, data_ = results } <- resultResponse res
      case rowTy of
        Syntax.TRecord tyInfo fields -> do
          origin <-
            maybe (error "impossible: record missing origin") pure $
            Check._typeInfoOrigin tyInfo
          tableName <-
            case origin of; Check.Row n -> pure n; _ -> error "impossible: record doesn't have Row origin"
          tableInfo <-
            maybe (error "impossible: missing table info") pure $
            Map.lookup (Check.toLower tableName) (Check._deTables env)
          let expectedNumColumns = Check._tiNumColumns tableInfo
          when (numColumns /= fromIntegral expectedNumColumns) . throw $
            ColumnMismatch expectedNumColumns numColumns rowTy
          values <- for results $ \row -> decodeRecord row fields
          pure $ Normalise.Many values
        _ -> do
          when (numColumns /= 1) . throw $
            ColumnMismatch 1 numColumns rowTy
          values <- for results $ \row -> decodeValue row rowTy
          pure $ Normalise.Many values
    Syntax.TQuery _ rowTy ->
      case rowTy of
        Syntax.TRecord _ fields -> do
          Result { rows = numRows, columns = numColumns, data_ = results } <- resultResponse res
          when (numRows > 1) . throw $ TooManyRows numRows rowTy
          let lfields = Vector.length fields
          when (numColumns /= fromIntegral lfields) . throw $
            ColumnMismatch lfields numColumns rowTy
          decodeRecord (results Vector.! 0) fields
        Syntax.TUnit _ -> do
          m_result <- resultOrDoneResponse res
          case m_result of
            Nothing -> pure ()
            Just result ->
              when (rows result > 1) . throw $ TooManyRows (rows result) rowTy
          pure Normalise.Unit
        _ -> do
          Result { rows = numRows, columns = numColumns, data_ = results } <- resultResponse res
          when (numRows > 1) . throw $ TooManyRows numRows rowTy
          when (numColumns /= 1) . throw $ ColumnMismatch 1 numColumns rowTy
          decodeValue (results Vector.! 0) rowTy
    _ -> error "impossible: query doesn't have type TQuery"

createTable :: QueryEnv -> Text -> IO ()
createTable (QueryEnv backend env) tableName = do
  let tableNameLower = Check.toLower tableName
  tableInfo <-
    maybe (throw $ TableNotFound tableName) pure $
    Map.lookup tableNameLower (Check._deTables env)
  let table = SQL.compileTable tableNameLower tableInfo
  doneResponse =<< Backend.request backend (Request'createTable table)

migrate :: forall typeInfo. Show typeInfo => Backend -> [Migration typeInfo] -> IO ()
migrate backend migrations = do
  let
    (parentGraph, fromVertex, toVertex) =
      Graph.graphFromEdges $
      (\migration ->
          (migration, Migration._mName migration, maybe [] pure $ Migration._mParent migration)
      ) <$>
      migrations
    childGraph = Graph.transposeG parentGraph
    -- in parentGraph, the edges go from child to parent
    -- in childGraph, the edges go from parent to child
  let
    roots :: [Migration.Name]
    roots =
      foldr
        (\v rest -> case fromVertex v of; (_, k, _) -> k : rest)
        []
        (Graph.roots childGraph fromVertex toVertex)
  migrationEnv <-
    foldlM
      (\acc next ->
        either (error . show) pure $
        Check.checkMigrations @typeInfo @Void migrations acc next
      )
      Check.emptyMigrationEnv
      roots
  let
    migrationOrder :: [Migration.Name]
    migrationOrder =
      fmap ((\(_, b, _) -> b) . fromVertex) .
      -- in `topSort`, vertex a is before vertex b when there is an edge from a to b
      Graph.topSort $
      -- parents should be run before children so we use `childGraph`
      childGraph
  for_ migrationOrder $ \migrationName -> do
    putStrLn $ "Running " <> show (Migration.unName migrationName) <> "..."
    let
      !compiled =
        SQL.compileMigration
          migrationEnv
          (case Map.lookup migrationName (Check._meMigrations migrationEnv) of
             Nothing -> error $ "Migration " <> show migrationName <> " missing from environment"
             Just migration -> migration
          )
    doneResponse =<< Backend.request backend (Request'migrate compiled)
