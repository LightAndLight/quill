module Test.Check.Migration (checkTests) where

import Test.Hspec (Spec, it, shouldBe)

checkTests :: Spec
checkTests = do
  it "1" $
    () `shouldBe` ()
