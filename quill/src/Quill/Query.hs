{-# language DeriveDataTypeable #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
module Quill.Query
  ( QueryEnv
  , load
  , query
  )
where

import qualified Bound.Scope as Bound
import Control.Exception (Exception, throw)
import Control.Monad (when)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Lens.Indexed (itraverse)
import Database.PostgreSQL.LibPQ (Connection)
import qualified Database.PostgreSQL.LibPQ as Postgres
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Void (absurd)

import qualified Quill.Check as Check
import Quill.Marshall (Value)
import qualified Quill.Marshall as Marshall
import Quill.Normalise (toExpr)
import qualified Quill.Normalise as Normalise
import qualified Quill.Parser as Parser
import qualified Quill.SQL as SQL
import qualified Quill.Syntax as Syntax

data QueryException
  = ParseError String
  | CheckError (Check.DeclError () ())
  | QueryNotFound Text
  | ArgumentMismatch Int Int
  | ArgumentCheckError Int (Check.TypeError Check.TypeInfo)
  | CompileError SQL.CompileError
  | DbError ByteString
  | UnknownError
  | TooManyRows Postgres.Row (Syntax.Type Check.TypeInfo)
  | ColumnMismatch Int Postgres.Column (Syntax.Type Check.TypeInfo)
  | DecodeError String
  deriving (Typeable, Show)
instance Exception QueryException

newtype QueryEnv = QueryEnv Check.DeclEnv

load :: FilePath -> IO QueryEnv
load path = do
  res <- Parser.parseFile ((,) <$> Parser.language <*> Parser.decls) path
  (lang, decls) <- either (throw . ParseError) pure res
  (_, declEnv) <-
    either
      (throw . CheckError)
      pure
      (Check.checkDecls (Check.emptyDeclEnv lang) $ Vector.fromList decls)
  pure $ QueryEnv declEnv

decodeValue ::
  Postgres.Result ->
  Postgres.Row ->
  Syntax.Type Check.TypeInfo ->
  IO Value
decodeValue res row ty = do
  m_val <- liftIO $ Postgres.getvalue' res row 0
  case m_val of
    Nothing ->
      error "impossible: getvalue' returned Nothing"
    Just val ->
      either (liftIO . throw . DecodeError) pure $
      Marshall.parseValue ty val

decodeRecord ::
  Postgres.Result ->
  Postgres.Row ->
  Vector (Text, Syntax.Type Check.TypeInfo) ->
  IO Value
decodeRecord res row a = evalStateT (go a) 0
  where
    goType :: Syntax.Type Check.TypeInfo -> StateT Postgres.Column IO Value
    goType ty = do
      col <- get
      m_val <- liftIO $ Postgres.getvalue' res row col
      put $ col + 1
      case m_val of
        Nothing ->
          error "impossible: getvalue' returned Nothing"
        Just val ->
          either (liftIO . throw . DecodeError) pure $
          Marshall.parseValue ty val

    go :: Vector (Text, Syntax.Type Check.TypeInfo) -> StateT Postgres.Column IO Value
    go fields =
      Normalise.Record <$> traverse (\(n, t) -> (,) n <$> goType t) fields

query :: Connection -> QueryEnv -> Text -> Vector Value -> IO Value
query conn (QueryEnv env) name args = do
  entry <-
    maybe (throw $ QueryNotFound name) pure $
    Map.lookup name (Check._deGlobalQueries env)
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
      (\i (v, t) -> either (throw . ArgumentCheckError i) pure $ Check.checkExpr env' (toExpr v) t)
      (Vector.zip args argTys)
  q <-
    either (throw . CompileError) pure .
    SQL.query env absurd .
    Normalise.normaliseExpr $
    Bound.instantiate (args' Vector.!) (Check._qeBody entry)
  let stmt = Lazy.toStrict . Builder.toLazyByteString $ SQL.compileQuery q
  res <-
    maybe (throw . maybe UnknownError DbError =<< Postgres.errorMessage conn) pure =<<
    Postgres.exec conn stmt
  numRows <- Postgres.ntuples res
  numColumns <- Postgres.nfields res
  case retTy of
    Syntax.TQuery _ (Syntax.TMany _ rowTy) -> do
      case rowTy of
        Syntax.TRecord tyInfo fields -> do
          origin <-
            maybe (error "impossible: record missing origin") pure $
            Check._typeInfoOrigin tyInfo
          tableName <-
            case origin of; Check.Row n -> pure n; _ -> error "impossible: record doesn't have Row origin"
          tableInfo <-
            maybe (error "impossible: missing table info") pure $
            Map.lookup tableName (Check._deTables env)
          let expectedNumColumns = Check._tiNumColumns tableInfo
          when (numColumns /= Postgres.toColumn expectedNumColumns) . throw $
            ColumnMismatch expectedNumColumns numColumns rowTy
          values <- for [0..numRows-1] $ \row -> decodeRecord res row fields
          pure $ Normalise.Many values
        _ -> do
          when (numColumns /= 1) . throw $
            ColumnMismatch 1 numColumns rowTy
          values <- for [0..numRows-1] $ \row -> decodeValue res row rowTy
          pure $ Normalise.Many values
    Syntax.TQuery _ rowTy -> do
      when (numRows > 1) . throw $ TooManyRows numRows rowTy
      case rowTy of
        Syntax.TRecord _ fields -> do
          let lfields = Vector.length fields
          when (numColumns /= Postgres.toColumn lfields) . throw $
            ColumnMismatch lfields numColumns rowTy
          decodeRecord res 0 fields
        _ -> do
          when (numColumns /= 1) . throw $ ColumnMismatch 1 numColumns rowTy
          decodeValue res 0 rowTy
    _ -> error "impossible: query doesn't have type TQuery"
