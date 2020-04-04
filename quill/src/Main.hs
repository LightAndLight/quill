{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Lens.TH (makeLenses)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Functor (void)
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket (send, recv)
import Options.Applicative
import System.Environment (lookupEnv)
import qualified System.IO as IO
import qualified System.Process as Process

data Command
  = DebugPlugin

data Config
  = Config
  { _cfgBackend :: String
  , _cfgDbHost :: Maybe String
  , _cfgDbPort :: Maybe String
  , _cfgDbName :: Maybe String
  , _cfgDbUser :: Maybe String
  , _cfgDbPassword :: Maybe String
  , _cfgCommand :: Command
  }
makeLenses ''Config

configParser :: Parser Config
configParser =
  Config <$>
  strOption (short 'b' <> metavar "BACKEND" <> help "Backend name") <*>
  optional (strOption $ long "host" <> metavar "HOST" <> help "Hostname for the connection") <*>
  optional (strOption $ long "port" <> metavar "PORT" <> help "Port for the connection") <*>
  optional (strOption $ long "db" <> metavar "DATABASE" <> help "Database name") <*>
  optional (strOption $ long "user" <> metavar "USER" <> help "Database username") <*>
  optional (strOption $ long "password" <> metavar "PASSWORD" <> help "Database password") <*>
  hsubparser
    (command "debug-plugin"
     (info debugPluginParser $
      fullDesc <> progDesc "Use the plugin in 'echo' mode for debugging purposes"
     )
    )
  where
    debugPluginParser :: Parser Command
    debugPluginParser =
      pure DebugPlugin

main :: IO ()
main = do
  config <-
    cfgDbPassword (\v -> (<|> v) <$> lookupEnv "QUILL_PASSWORD") =<<
    cfgDbUser (\v -> (<|> v) <$> lookupEnv "QUILL_USER") =<<
    cfgDbName (\v -> (<|> v) <$> lookupEnv "QUILL_DATABASE") =<<
    cfgDbPort (\v -> (<|> v) <$> lookupEnv "QUILL_PORT") =<<
    cfgDbHost (\v -> (<|> v) <$> lookupEnv "QUILL_HOST") =<<
    execParser (info (helper <*> configParser) fullDesc)
  addr <-
    head <$>
    Socket.getAddrInfo
      (Just $
       Socket.defaultHints
       { Socket.addrFlags = [Socket.AI_PASSIVE]
       , Socket.addrFamily = Socket.AF_INET
       , Socket.addrSocketType = Socket.Stream
       }
      )
      (Just "127.0.0.1")
      (Just "0")
  bracket
    (Socket.socket
       (Socket.addrFamily addr)
       (Socket.addrSocketType addr)
       (Socket.addrProtocol addr)
    )
    Socket.close
    (\init_sock -> do
       Socket.bind init_sock (Socket.addrAddress addr)
       port <-
         (\case; Socket.SockAddrInet port _ -> pure port; _ -> error "Got non-IPv4 socket address") =<<
         Socket.getSocketName init_sock
       print =<< Socket.getSocketName init_sock
       putStrLn "Listening..."
       Socket.listen init_sock 5
       putStrLn "Forking..."
       forkBackend port config
       bracket
         (do
            putStrLn "Waiting for connection..."
            fst <$> Socket.accept init_sock
         )
         Socket.close
         (\sock -> do
            putStrLn "Starting server..."
            server (_cfgCommand config) sock
         )
    )

forkBackend :: Socket.PortNumber -> Config -> IO ()
forkBackend port config =
  void . forkIO $ do
    Process.callProcess ("quill-" <> _cfgBackend config) $
      ["--port", show port] <>
      withArg "--db-host" _cfgDbHost <>
      withArg "--db-port" _cfgDbPort <>
      withArg "--db-name" _cfgDbName <>
      withArg "--db-user" _cfgDbUser <>
      withArg "--db-password" _cfgDbUser
  where
    withArg name get =
      maybe [] (\arg -> [name, arg]) (get config)

server :: Command -> Socket -> IO ()
server cmd sock =
  case cmd of
    DebugPlugin ->
      let
        loop = do
          input <- putStr "input: " *> IO.hFlush IO.stdout *> Char8.getLine
          count <- Socket.send sock input
          output <- Socket.recv sock count
          Char8.putStrLn $ "output: " <> output
      in
        loop
