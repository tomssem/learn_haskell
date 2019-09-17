module SaddleBackSpec (spec) where

import SaddleBack
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "SaddleBack" $
    describe "saddleback" $ do
      it "works for addition" $
        invert (+) 10 `shouldBe` [(0,10),(1,9),(2,8),(3,7),(4,6),(5,5),(6,4),(7,3),(8,2),(9,1),(10,0)]
      it "works for multiplication" $
        invert (*) 10 `shouldBe` [(1,10),(2,5),(5,2),(10,1)]
