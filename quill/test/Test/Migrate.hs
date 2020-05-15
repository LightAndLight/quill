module Test.Migrate (migrateTests) where

import Capnp.Gen.Request.Pure (Request(..))
import Capnp.Gen.Response.Pure (Response)
import Data.ByteString (ByteString)
import Test.Hspec

import Database (setupDb)
import Expectations (shouldBeDone)
import Quill.Backend (Backend)
import qualified Quill.Backend as Backend
import qualified Quill.Query as Query

exec :: Backend -> ByteString -> IO Response
exec backend = Backend.request backend . Request'exec

migrateTests :: Spec
migrateTests = do
  around (setupDb (const $ pure ()) (\backend -> _)) . describe "migrate" $ do
    it "1" $ \backend -> do
      () `shouldBe` ()
