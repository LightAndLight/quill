{-| A (non-compliant) dummy backend that only knows how to echo requests
-}
module Main where

import Control.Exception (bracket)
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket (send, recv)
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
  putStrLn $ "Port: " <> _cfgPort config
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
       putStrLn "Connecting..."
       Socket.connect sock (Socket.addrAddress addr)
       putStrLn "Connected!"
       runEcho sock
    )

runEcho :: Socket -> IO ()
runEcho sock = do
  putStrLn "Waiting for input"
  input <- Socket.recv sock 4096
  putStrLn "Recieved input"
  _ <- Socket.send sock input
  pure ()
