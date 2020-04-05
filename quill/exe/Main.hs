{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Main where

import qualified Capnp (defaultLimit, sGetValue, sPutValue)
import qualified Capnp.Gen.Request.Pure as Request
import qualified Capnp.Gen.Response.Pure as Response
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Lens.TH (makeLenses)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Functor (void)
import GHC.Exts (fromString)
import Network.Socket (Socket)
import qualified Network.Socket as Socket
import Options.Applicative
import System.Environment (lookupEnv)
import qualified System.IO as IO
import qualified System.Process as Process
import Text.Read (readMaybe)

import Quill.Backend (Backend)
import qualified Quill.Backend as Backend

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
  m_port <-
    case _cfgDbPort config of
      Nothing -> pure Nothing
      Just str ->
        case readMaybe str of
          Nothing -> error $ "'" <> str <> "' is not a number"
          Just port -> pure $ Just port
  Backend.withBackendProcess
    (fromString $ "quill-" <> _cfgBackend config)
    (Backend.Config
     { Backend._cfgDbHost = fromString <$> _cfgDbHost config
     , Backend._cfgDbPort = m_port
     , Backend._cfgDbName = fromString <$> _cfgDbName config
     , Backend._cfgDbUser = fromString <$> _cfgDbUser config
     , Backend._cfgDbPassword = fromString <$> _cfgDbPassword config
     }
    )
    (server $ _cfgCommand config)

server :: Command -> Backend -> IO ()
server cmd backend =
  case cmd of
    DebugPlugin ->
      let
        loop = do
          input <- putStr "input: " *> IO.hFlush IO.stdout *> Char8.getLine
          case input of
            "quit" -> pure ()
            _ -> do
              res <- Backend.request backend $ Request.Request'echo input
              case res of
                Response.Response'echo output -> Char8.putStrLn $ "output: " <> output
                _ -> do
                  putStrLn $ "Unexpected response: " <> show res
              loop
      in
        loop
