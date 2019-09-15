module SurpassingSpec (spec) where

import Data.List
import Surpassing
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "Surpassing" $ do
    describe "msc" $ do
      it "returns Nothing when empty list is given" $
        msc ([] :: [Int]) `shouldBe` Nothing
      it "returns 0 when single element list is given" $
        msc [0] `shouldBe` Just 0
      it "returns 0 when array in ascending order is given" $
        msc [1, 2, 3, 4] `shouldBe` Just 0
      it "returns length of list minus one when given contiguous descending list" $
        msc [6, 5, 4, 3, 2] `shouldBe` Just 4
    describe "myTails" $
      it "is same as tails with empty list filtered out" $ property $
        \x -> (myTails x) == (filter (not . null) $ tails (x :: [Int]))
