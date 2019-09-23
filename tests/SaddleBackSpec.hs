module SaddleBackSpec (spec) where

import Data.List

import Test.Hspec
import Test.QuickCheck

import SaddleBack

almostMultiplication :: Int -> Int -> Int
almostMultiplication x y = (x + 1) * (y + 1)

spec :: Spec
spec =
  describe "SaddleBack" $ do
    describe "invert" $ do
      it "works for addition" $
        invert (+) 10 `shouldBe` [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]
      it "works for 'multiplication'" $
        invert almostMultiplication 10 `shouldBe` [(0,9),(1,4),(4,1),(9,0)]
    describe "invertFaster" $ do
      it "should be the same as invert for addition" $ property $
        \x -> invert (+) x == invertFaster (+) x
      it "should be the same as invert for 'multiplication'" $ property $
        \x -> invert almostMultiplication x == invertFaster almostMultiplication x
    describe "invertFancy" $ do
      it "should be the same as invert for addition" $ property $
        \x -> invert (+) x == invertFancy (+) x
      it " should be the as invert for 'multiplication" $ property $
        \x -> invert almostMultiplication x == invertFancy almostMultiplication x
