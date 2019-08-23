--
-- Implementation of Chapter 1 of "Pearls of Functional Algorithm Design"
--

module SmallestFreeNumber where

import Data.Array
import Numeric.Natural
import Control.Exception

type Nat = Int

testList = [08, 23, 09, 00, 12, 11 , 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 03, 06] :: [Nat]

minfree1 :: [Nat] -> Nat
minfree1 xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

checklist :: [Nat] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) (map fromIntegral xs)) (repeat True))
              where n = length xs

countlist :: [Nat] -> Array Nat Nat
countlist xs = accumArray (+) 0 (0, n) (zip (map fromIntegral xs) (repeat 1))
              where n = foldl max 0 xs -- 0 is identity element for max on natural numbers

countsort :: [Nat] -> Maybe [Nat]
countsort [] = Just []
countsort xs = if any (< 0) xs 
               then Nothing
               else Just $ concat [replicate (fromIntegral k) x | (x, k) <- assocs (countlist xs)]
