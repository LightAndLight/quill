{-# language PackageImports #-}
import Distribution.Simple

import qualified Data.List as List
import System.Directory (makeAbsolute, listDirectory)
import "quill-plugin-api-setup" Setup (capnp, find_capnp)

main :: IO ()
main =
  defaultMainWithHooks $
  simpleUserHooks
  { postConf = \args config desc info -> do
      postConf simpleUserHooks args config desc info
      () <$ find_capnp
  , preBuild = \args flags -> do
      res <- preBuild simpleUserHooks args flags
      schemaPath <- makeAbsolute "./schemas"
      schemas <- filter (".capnp" `List.isSuffixOf`) <$> listDirectory schemaPath
      capnp schemaPath schemas
      pure res
  }
