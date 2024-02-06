module LibSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "test example" $
    it "does a test" $
      () `shouldBe` ()
