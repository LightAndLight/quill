{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
module Main where

import qualified Capnp.Gen.Request.Pure as Request
import qualified Capnp.Gen.Response.Pure as Response
import Control.Lens.TH (makeLenses)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (foldlM, for_)
import qualified Data.Graph as Graph
import qualified Data.Graph.Extra as Graph (roots)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Void (Void)
import GHC.Exts (fromString)
import Options.Applicative
import qualified System.Directory as Directory
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import qualified System.IO as IO
import Text.Read (readMaybe)

import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import qualified Quill.Check.Migration as Check
import qualified Quill.Parser as Parser (parseFile, eof)
import qualified Quill.Parser.Migration as Parser (migration)
import qualified Quill.SQL.Migration as SQL
import qualified Quill.Syntax.Migration as Migration

quillfiles :: FilePath
quillfiles = "./quillfiles"

data Command
  = DebugPlugin
  | Migrate

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
     ) <>
     command "migrate"
     (info migrateParser $
      fullDesc <> progDesc "Migrate a database"
     )
    )
  where
    debugPluginParser :: Parser Command
    debugPluginParser =
      pure DebugPlugin

    migrateParser :: Parser Command
    migrateParser =
      pure Migrate

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
      Just portStr ->
        case readMaybe portStr of
          Nothing -> error $ "'" <> portStr <> "' is not a number"
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
    Migrate -> do
      migrationsFile <- Directory.makeAbsolute $ quillfiles </> "migrations.quillm"
      migrations <-
        either error pure =<<
        Parser.parseFile (some Parser.migration <* Parser.eof) migrationsFile
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
            Check.checkMigrations @() @Void migrations acc next
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
              (Maybe.fromJust $ Map.lookup migrationName (Check._meMigrations migrationEnv))
        res <- Backend.request backend $ Request.Request'migrate compiled
        case res of
          Response.Response'done -> pure ()
          Response.Response'error err -> do
            putStrLn . Char8.unpack $ err
            exitFailure
          _ -> do
            putStrLn $ "Unexpected response from backend: " <> show res
            exitFailure
      putStrLn "All migrations successful!"
      exitSuccess
