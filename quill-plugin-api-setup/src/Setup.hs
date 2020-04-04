module Setup (capnp) where

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import System.Directory (findExecutable, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.Exit (exitFailure)
import System.Process (callProcess)

capnp ::
  FilePath -> -- path to schema files
  [String] -> -- schema file names
  IO ()
capnp schemaPath schemas = do
  m_path <- findExecutable "capnp"
  case m_path of
    Nothing -> do
      putStrLn "quill-plugin-api-setup: missing executable 'capnp'"
      exitFailure
    Just path ->
      withCurrentDirectory schemaPath .
      bracket
        (lookupEnv "PWD" >>= maybe (pure Nothing) (\val -> Just val <$ setEnv "PWD" schemaPath))
        (traverse_ $ setEnv "PWD") $
      \_ -> do
        putStrLn "Compiling Cap'n Proto schemas..."
        callProcess path $ ["compile", "-ohaskell"] <> schemas
