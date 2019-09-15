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
      it "returns 0 when array in descending order is given" $
        msc [1, 2, 3, 4] `shouldBe` Just 3
      it "returns length of list minus one when given contiguous ascending list" $
        msc [6, 5, 4, 3, 2] `shouldBe` Just 0
    describe "scount" $ do
      it "returns 0 for an empty list" $
        scount 0 [] `shouldBe` 0
      it "returns 0 when value is bigger than all elements in list" $
        scount 4 [0, 2, 3, 1] `shouldBe` 0
      it "returns 0 when list counts elements all the same as value" $
        scount 10 (replicate 100 10) `shouldBe` 0
      it "returns length of list when all values are greater" $
        scount 0 [1..100] == 100
    describe "myTails" $
      it "is same as tails with empty list filtered out" $ property $
        \x -> (myTails x) == (filter (not . null) $ tails (x :: [Int]))
