{-# language OverloadedLists, OverloadedStrings #-}
module Main where

import qualified Capnp (defaultLimit, sGetValue, sPutValue)
import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response(..), Result(Result))
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Traversable (for)
import Database.PostgreSQL.LibPQ (Connection)
import qualified Database.PostgreSQL.LibPQ as Postgres
import GHC.Exts (fromString)
import Network.Socket (Socket, AddrInfo)
import qualified Network.Socket as Socket
import Options.Applicative

data Config
  = Config
  { _cfgPort :: String
  , _cfgDbHost :: Maybe String
  , _cfgDbPort :: Maybe String
  , _cfgDbName :: Maybe String
  , _cfgDbUser :: Maybe String
  , _cfgDbPassword :: Maybe String
  }

configParser :: Parser Config
configParser =
  Config <$>
  strOption (long "port" <> metavar "PORT" <> help "Communication port") <*>
  optional (strOption $ long "db-host" <> metavar "DB_HOST" <> help "Hostname for the connection") <*>
  optional (strOption $ long "db-port" <> metavar "DB_PORT" <> help "Port for the connection") <*>
  optional (strOption $ long "db-db" <> metavar "DB_DATABASE" <> help "Database name") <*>
  optional (strOption $ long "db-user" <> metavar "DB_USER" <> help "Database username") <*>
  optional (strOption $ long "db-password" <> metavar "DB_PASSWORD" <> help "Database password")

withSocket :: AddrInfo -> (Socket -> IO ()) -> IO ()
withSocket addr =
  bracket
    (Socket.socket
       (Socket.addrFamily addr)
       (Socket.addrSocketType addr)
       (Socket.addrProtocol addr)
    )
    Socket.close

withConnection :: Config -> (Connection -> IO ()) -> IO ()
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

main :: IO ()
main = do
  config <- execParser (info (helper <*> configParser) fullDesc)
  addr <-
    head <$>
    Socket.getAddrInfo
      (Just $
       Socket.defaultHints
       { Socket.addrFamily = Socket.AF_INET
       , Socket.addrSocketType = Socket.Stream
       }
      )
      (Just "127.0.0.1")
      (Just $ _cfgPort config)
  withSocket
    addr
    (\sock -> do
      Socket.connect sock (Socket.addrAddress addr)
      run config sock
    )

run :: Config -> Socket -> IO ()
run config sock = loop
  where
    loop = do
      req <- Capnp.sGetValue sock Capnp.defaultLimit
      quit <- handleRequest config sock req
      unless quit loop

handleRequest ::
  Config ->
  Socket ->
  Request ->
  IO Bool -- should we quit?
handleRequest config sock req =
  case req of
    Request'quit ->
      quit
    Request'exec input -> do
      withConnection config $ \conn -> do
        m_res <- Postgres.exec conn input
        case m_res of
          Nothing -> respondDbError conn
          Just res -> do
            status <- Postgres.resultStatus res
            case status of
              Postgres.CommandOk -> respond Response'done
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
      respond . Response'error . fromString $ "Unexpected union tag: " <> show tag
      continue
  where
    respondError = respond . Response'error
    respondDbError conn = do
      m_err <- Postgres.errorMessage conn
      case m_err of
        Nothing -> respondError "An unknown error occurred"
        Just err -> respondError err
    respond = Capnp.sPutValue sock
    continue = pure False
    quit = pure True
