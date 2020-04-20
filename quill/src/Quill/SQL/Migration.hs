module Quill.SQL.Migration (compileMigration) where

import qualified Capnp.Gen.Migration.Pure as SQL (Migration(..))
import Quill.Check (TypeInfo)
import Quill.Syntax.Migration (Migration)

compileMigration :: Migration TypeInfo -> SQL.Migration
compileMigration = _
