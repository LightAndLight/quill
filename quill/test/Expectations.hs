module Expectations where

import Test.Hspec
import Capnp.Gen.Response.Pure (Response(..), Result)

shouldBeDone :: HasCallStack => Response -> IO ()
shouldBeDone res =
  case res of
    Response'done -> pure ()
    _ -> error $ "Expected 'done', got: " <> show res

shouldBeResult :: HasCallStack => Response -> IO Result
shouldBeResult res =
  case res of
    Response'result res' -> pure res'
    _ -> error $ "Expected result, got: " <> show res
