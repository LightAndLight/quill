module Setup (find_capnp, capnp) where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import System.Directory (findExecutable, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure)
import System.Process (callProcess)

find_capnp :: IO FilePath
find_capnp = do
  m_path <- findExecutable "capnp"
  case m_path of
    Nothing -> do
      putStrLn "quill-plugin-api-setup: missing executable 'capnp'"
      exitFailure
    Just path -> pure path

capnp ::
  FilePath -> -- path to schema files
  [String] -> -- schema file names
  IO ()
capnp schemaPath schemas = do
  path <- find_capnp
  withCurrentDirectory schemaPath .
    bracket
      (lookupEnv "PWD" >>= maybe (pure Nothing) (\val -> Just val <$ setEnv "PWD" schemaPath))
      (traverse_ $ setEnv "PWD") $
    \_ -> do
      putStrLn "Compiling Cap'n Proto schemas..."
      callProcess path $ ["compile", "-ohaskell"] <> schemas
