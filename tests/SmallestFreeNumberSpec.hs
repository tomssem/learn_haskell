module SmallestFreeNumberSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SmallestFreeNumber
import Data.Array

mysort :: [Nat] -> Maybe [Nat]
mysort [] = Just []
mysort (p:xs) = if any (< 0) (p:xs) 
                then Nothing
                else Just $ as ++ [p] ++ bs
						          where
						          	as = filter (<= p) xs
						          	bs = filter (> p) xs

spec :: Spec
spec =
  describe "SmallestFreeNumber" $ do
    describe "minfree1" $ do
      it "returns 0 when given empty list" $
        minfree1 [] `shouldBe` 0
      it "returns 1 when given a list containined only 1" $
        minfree1 [0] `shouldBe` 1
      it "returns 0 when given a list containing any list of numbers greater than 1" $
        minfree1 [1] `shouldBe` 0

    describe "countlist" $ do
      it "Returns array length 1 consisting of False when given empty array" $
        checklist [] `shouldBe` listArray (0, 0) [False]
      it "returns array of [True, False] when xs consists of a single 0" $
        checklist [0] `shouldBe` listArray (0, 1) [True, False]
      it "returns array of [False, True] when xs consists of a single 0" $
        checklist [1] `shouldBe` listArray (0, 1) [False, True]
      it "returns correct array for longer input" $
        checklist [1, 3, 5] `shouldBe` listArray (0, 3) [False, True, False, True, False]

    describe "read" $
	  	it "is inverse to show" $ property $
	  		\x -> (read . show) x == (x :: Int)

    describe "countsort" $
      it "is same as 'sort'" $ property $
        \x -> countsort x == mysort x
