{-# language BangPatterns #-}
{-# language OverloadedLists, OverloadedStrings #-}
{-# language ViewPatterns #-}
module Quill.Backend.Postgres (Config(..), run) where

import qualified Capnp (defaultLimit, sGetValue, sPutValue)
import Capnp.Gen.Migration.Pure
  ( AlterTable(..), Command(..)
  , Migration(Migration), Migration'parent(..)
  , ParentInfo(ParentInfo)
  , TableChange(..)
  )
import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..), Result(Result))
import Capnp.Gen.Table.Pure
  ( Column(Column)
  , Constraint(..)
  , Other(Other)
  , Table(Table)
  )
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Word (Word16)
import Database.PostgreSQL.LibPQ (Connection)
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.Exts (fromString)
import Network.Socket (Socket)

data Config
  = Config
  { _cfgPort :: String
  , _cfgDbHost :: Maybe String
  , _cfgDbPort :: Maybe String
  , _cfgDbName :: Maybe String
  , _cfgDbUser :: Maybe String
  , _cfgDbPassword :: Maybe String
  }

-- Don't return the Connection!
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config = bracket (Postgres.connectdb connString) Postgres.finish
  where
    item :: ByteString -> (Config -> Maybe String) -> [ByteString]
    item label get = maybe [] (\val -> [label <> "=" <> Char8.pack val]) $ get config

    connString =
      Char8.unwords $
      item "host" _cfgDbHost <>
      item "port" _cfgDbPort <>
      item "dbname" _cfgDbName <>
      item "user" _cfgDbUser <>
      item "password" _cfgDbPassword

run :: Socket -> Config -> IO ()
run sock config = loop
  where
    loop = do
      req <- Capnp.sGetValue sock Capnp.defaultLimit
      quit <- handleRequest config sock req
      unless quit loop

gotUnknown :: String -> Word16 -> a
gotUnknown ctor tag = error $ "Unknown tag in " <> ctor <> ": " <> show tag

data QueryPart
  = I ByteString
  | V ByteString
  | S ByteString

exec :: Connection -> [QueryPart] -> IO (Maybe Postgres.Result)
exec conn ps =
  runMaybeT $ do
    query <-
      Lazy.toStrict . Builder.toLazyByteString . fold <$>
      traverse (fmap Builder.byteString . queryPart) ps
    MaybeT $ Postgres.exec conn query
  where
    queryPart (I s) = pure s
    queryPart (V s) = MaybeT $ Postgres.escapeStringConn conn s
    queryPart (S s) = pure s

mkSeqName :: ByteString -> ByteString -> ByteString
mkSeqName tableName colName =
  tableName <> "_" <>
  colName <> "_seq"

createTable :: Table -> [QueryPart]
createTable (Table tableName columns constraints) =
  createSequences columns <>
  [S "CREATE TABLE ", I tableName, S "(\n" ] <>
  fold
    (List.intersperse [S ",\n"] $
     foldr ((:) . createColumn) [] columns
    ) <>
  foldMap ((S ",\n" :) . createConstraint) constraints <>
  [S ");"] <>
  alterSequences columns
  where
    createSequences :: Vector Column -> [QueryPart]
    createSequences =
      foldMap
        (\(Column colName _ _ autoIncrement) ->
          if autoIncrement
          then
            [ S "CREATE SEQUENCE "
            , I $ mkSeqName tableName colName
            , S ";\n"
            ]
          else
            []
        )
    alterSequences :: Vector Column -> [QueryPart]
    alterSequences =
      foldMap
        (\(Column colName _ _ autoIncrement) ->
          if autoIncrement
          then
            [ S "ALTER SEQUENCE "
            , I $ mkSeqName tableName colName
            , S " OWNED BY "
            , I tableName
            , S "."
            , I colName
            , S ";\n"
            ]
          else
            []
        )

    createColumn :: Column -> [QueryPart]
    createColumn (Column colName colTy notNull autoIncrement) =
      [ I colName
      , S " "
      , V colTy
      ] <>
      (if notNull then [S " NOT NULL"] else []) <>
      (if autoIncrement
       then
         [ S " DEFAULT nextval('"
         , I $ mkSeqName tableName colName
         , S "')"
         ]
       else []
      )

    createConstraint :: Constraint -> [QueryPart]
    createConstraint c =
      case c of
        Constraint'primaryKey args ->
          [S "PRIMARY KEY("] <>
          List.intersperse (S ",") (foldr ((:) . I) [] args) <>
          [S ")"]
        Constraint'autoIncrement{} ->
          -- autoincrement should have been set on a per-column basis
          mempty
        Constraint'other (Other otherName args) -> error "can't compile Other" otherName args
        Constraint'unknown' tag -> gotUnknown "Constraint" tag

compileChange :: ByteString -> TableChange -> [QueryPart]
compileChange tableName change =
  case change of
    TableChange'unknown' tag -> gotUnknown "TableChange" tag
    TableChange'addColumn (Column colName colType notNull autoIncrement) ->
      let
        seqName = mkSeqName tableName colName
      in
        (if autoIncrement
         then [S "CREATE SEQUENCE ", I seqName, S ";\n"]
         else mempty
        ) <>
        [S "ALTER TABLE ", I tableName, S " ADD ", I colName, S " ", V colType] <>
        (if notNull then [S " NOT NULL"] else []) <>
        (if autoIncrement
         then [S " DEFAULT nextval('", I seqName, S "')"]
         else []
        ) <>
        [S ";\n"] <>
        (if autoIncrement
         then [S "ALTER SEQUENCE ", I seqName, S " OWNED BY ", I tableName, S ".", I colName, S ";\n"]
         else mempty
        )
    TableChange'dropColumn colName ->
      [S "ALTER TABLE ", I tableName, S " DROP COLUMN ", I colName, S ";\n"]
    TableChange'addConstraint constr ->
      case constr of
        Constraint'unknown' tag -> gotUnknown "Constraint" tag
        Constraint'primaryKey args ->
          let
            constraintName = I $ tableName <> "_" <> "pk"
          in
            [ S "ALTER TABLE ", I tableName, S " ADD CONSTRAINT ", constraintName, S " "
            , S "PRIMARY KEY ("
            ] <> List.intersperse (S ", ") (foldr ((:) . I) [] args) <>
            [ S ")", S ";\n"]
        Constraint'autoIncrement colName ->
          let
            seqName = mkSeqName tableName colName
          in
            [ S "CREATE SEQUENCE ", I seqName, S ";\n"
            , S "ALTER TABLE ", I tableName, S " ALTER COLUMN ", I colName
            , S " SET DEFAULT nextval('", I seqName, S "');\n"
            , S "ALTER SEQUENCE ", I seqName, S " OWNED BY ", I tableName, S ".", I colName, S ";\n"
            ]
        Constraint'other (Other constrName args) -> error "can't compile Other" constrName args

compileCommand :: Command -> [QueryPart]
compileCommand command =
  case command of
    Command'unknown' tag -> gotUnknown "Command" tag
    Command'createTable table -> createTable table
    Command'alterTable (AlterTable tableName tableChanges) ->
      foldMap (compileChange tableName) tableChanges

escapeStringConn :: Postgres.Connection -> ByteString -> IO ByteString
escapeStringConn conn val =
  maybe (error "internal error: escapeStringConn failed") pure =<<
  Postgres.escapeStringConn conn val

handleRequest ::
  Config ->
  Socket ->
  Request ->
  IO Bool -- should we quit?
handleRequest config sock req =
  case req of
    Request'quit -> do
      respond Response'quitting
      quit
    Request'createTable table -> do
      let !tableQuery = createTable table
      withConnection config $ \conn -> do
        m_res <- exec conn tableQuery
        case m_res of
          Nothing -> respondDbError conn
          Just res -> do
            status <- Postgres.resultStatus res
            case status of
              Postgres.CommandOk -> respondDone
              _ -> respondDbError conn
      continue
    Request'migrate migration -> handleMigration migration
    Request'exec input -> do
      withConnection config $ \conn -> do
        m_res <- Postgres.exec conn input
        case m_res of
          Nothing -> respondDbError conn
          Just res -> do
            status <- Postgres.resultStatus res
            case status of
              Postgres.CommandOk -> respondDone
              Postgres.TuplesOk -> do
                numRows@(Postgres.Row rs) <- Postgres.ntuples res
                numCols@(Postgres.Col cs) <- Postgres.nfields res
                e_rows <-
                  runExceptT .
                  for [0..numRows-1] $ \row ->
                  for [0..numCols-1] $ \col -> do
                    m_val <- liftIO $ Postgres.getvalue res row col
                    case m_val of
                      Nothing -> throwError (row, col)
                      Just val -> pure val
                case e_rows of
                  Left pos -> respondError $ "Missing value at " <> Char8.pack (show pos)
                  Right rows -> respond $ Response'result (Result (fromIntegral rs) (fromIntegral cs) rows)
              Postgres.SingleTuple -> do
                numCols@(Postgres.Col n) <- Postgres.nfields res
                e_cols <-
                  runExceptT $
                  for [0..numCols-1] $ \col -> do
                    m_val <- liftIO $ Postgres.getvalue res 0 col
                    case m_val of
                      Nothing -> throwError col
                      Just val -> pure val
                case e_cols of
                  Left col -> respondError $ "Missing value at " <> Char8.pack (show col)
                  Right cols -> respond $ Response'result (Result 1 (fromIntegral n) [cols])
              Postgres.CopyIn -> respondError "Unsupported response: CopyIn"
              Postgres.CopyOut -> respondError "Unsupported response: CopyOut"
              Postgres.CopyBoth -> respondError "Unsupported response: CopyBoth"
              _ -> respondDbError conn
      continue
    Request'echo input -> do
      respond $ Response'echo input
      continue
    Request'unknown' tag -> do
      respondError . fromString $ "Unexpected union tag: " <> show tag
      continue
  where
    respond = Capnp.sPutValue sock
    respondDone = respond Response'done
    respondError = respond . Response'error
    respondDbError conn = do
      m_err <- Postgres.errorMessage conn
      case m_err of
        Nothing -> respondError "An unknown error occurred"
        Just err -> respondError err

    withResult ::
      Connection ->
      IO (Maybe a) ->
      (a -> IO Bool) ->
      IO Bool
    withResult conn m k = do
      m_res <- m
      case m_res of
        Nothing -> do
          respondDbError conn
          continue
        Just res -> k res

    continue = pure False
    quit = pure True

    runMigration :: Connection -> ByteString -> ByteString -> Vector Command -> IO Bool
    runMigration conn migrationName migrationHash commands =
      let
        query =
          [ S "INSERT INTO quill_migrations(name, hash) VALUES ("
          , S "'", V migrationName, S "'"
          , S ", "
          , S "'", V migrationHash, S "'"
          , S ");\n"
          ] <>
          foldMap compileCommand commands
      in
        withResult conn (exec conn query) $ \result -> do
          status <- Postgres.resultStatus result
          case status of
            Postgres.CommandOk -> respondDone
            Postgres.FatalError -> respondDbError conn
            _ -> respondError $ "Unsupported response: " <> Char8.pack (show status)
          continue

    withMigrationHash :: Connection -> ByteString -> (Maybe ByteString -> IO Bool) -> IO Bool
    withMigrationHash conn migrationName k = do
      escapedName <- escapeStringConn conn migrationName
      let
        selectName =
          Postgres.exec conn $
          "SELECT hash FROM quill_migrations " <>
          "WHERE name = '" <> escapedName <> "';"
      withResult conn selectName $ \result -> do
        status <- Postgres.resultStatus result
        case status of
          Postgres.TuplesOk -> do
            hashRows <- Postgres.ntuples result
            case compare hashRows 1 of
              LT -> k Nothing
              EQ -> do
                m_value <- Postgres.getvalue result 0 0
                case m_value of
                  Nothing -> error "internal error: no hash data returned from database"
                  Just value -> k $ Just value
              GT -> do
                respondError $
                  "Migration " <>
                  migrationName <>
                  " has multiple entries in the migration table"
                continue
          Postgres.FatalError -> do
            respondDbError conn
            continue
          _ -> do
            respondError $ "Unsupported response: " <> Char8.pack (show status)
            continue

    handleMigration :: Migration -> IO Bool
    handleMigration (Migration migrationName migrationHash m_parent commands) =
      case m_parent of
        Migration'parent'unknown' tag -> gotUnknown "Migration.parent" tag
        Migration'parent'none ->
          withConnection config $ \conn ->
            let
              createMigrations =
                Postgres.exec conn $
                "CREATE TABLE IF NOT EXISTS quill_migrations(" <>
                "id SERIAL PRIMARY KEY, name TEXT NOT NULL, hash TEXT NOT NULL" <>
                ");"
            in
            withResult conn createMigrations $ \result -> do
              status <- Postgres.resultStatus result
              case status of
                Postgres.CommandOk ->
                  withMigrationHash conn migrationName $ \m_hash ->
                  case m_hash of
                    Nothing -> runMigration conn migrationName migrationHash commands
                    Just hash ->
                      if hash == migrationHash
                      then respondDone *> continue
                      else do
                        respondError $ "Migration " <> migrationName <> " is registered with a different hash"
                        continue
                Postgres.FatalError -> do
                  respondDbError conn
                  continue
                _ -> do
                  respondError $ "Unsupported response: " <> Char8.pack (show status)
                  continue
        Migration'parent'some (ParentInfo parentName parentHash) ->
          withConnection config $ \conn ->
          withMigrationHash conn parentName $ \m_parentHash ->
          case m_parentHash of
            Nothing -> do
              respondError $ "Missing parent migration '" <> parentName <> "'"
              continue
            Just actualParentHash ->
              if parentHash == actualParentHash
              then runMigration conn migrationName migrationHash commands
              else do
                respondError $ "Parent of " <> migrationName <> " is registered with a different hash"
                continue
