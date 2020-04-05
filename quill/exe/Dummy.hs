{-| A (non-compliant) dummy backend that only knows how to echo requests
-}
module Main where

import qualified Capnp (defaultLimit, sGetValue, sPutValue)
import qualified Capnp.Gen.Request.Pure as Request
import qualified Capnp.Gen.Response.Pure as Response
import Control.Exception (bracket)
import Network.Socket (Socket)
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
  bracket
    (Socket.socket
       (Socket.addrFamily addr)
       (Socket.addrSocketType addr)
       (Socket.addrProtocol addr)
    )
    Socket.close
    (\sock -> do
       Socket.connect sock (Socket.addrAddress addr)
       runEcho sock
    )

runEcho :: Socket -> IO ()
runEcho sock = go
  where
    go = do
      req <- Capnp.sGetValue sock Capnp.defaultLimit
      case req of
        Request.Request'echo input' -> do
          Capnp.sPutValue sock $ Response.Response'echo input'
          go
        Request.Request'quit ->
          Capnp.sPutValue sock Response.Response'quitting
        _ -> do
          putStrLn $ "Unexpected request: " <> show req
          go
