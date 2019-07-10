module SmallestFreeNumberSpec (spec) where

import Test.Hspec
import SmallestFreeNumber

spec :: Spec
spec =
  describe "minfree1" $ do
    it "returns 0 when given empty list" $
      minfree1 [] `shouldBe` 0
    it "returns 1 when given a list containined only 1" $
      minfree1 [0] `shouldBe` 1
    it "returns 0 when given a list containing any list of numbers greater than 1" $
      minfree1 [1] `shouldBe` 0
