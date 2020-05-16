{-# language OverloadedLists, OverloadedStrings #-}
module Main where

import Control.Exception (bracket)
import Network.Socket (AddrInfo, Socket)
import qualified Network.Socket as Socket
import Options.Applicative

import Quill.Backend.Postgres (Config(..), run)

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
      run sock config
    )
