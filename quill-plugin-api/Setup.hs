{-# language PackageImports #-}
import Distribution.Simple

import qualified Data.List as List
import System.Directory (makeAbsolute, listDirectory)
import "quill-plugin-api-setup" Setup (capnp)

main :: IO ()
main =
  defaultMainWithHooks $
  simpleUserHooks
  { postConf = \args config desc info -> do
      postConf simpleUserHooks args config desc info
      schemaPath <- makeAbsolute "./schemas"
      schemas <- filter (".capnp" `List.isSuffixOf`) <$> listDirectory schemaPath
      capnp schemaPath schemas
  }
