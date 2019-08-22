--
-- Implementation of Chapter 1 of "Pearls of Functional Algorithm Design"
--

module SmallestFreeNumber where

import Data.Array

type Nat = Int

testList = [08, 23, 09, 00, 12, 11 , 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 03, 06] :: [Nat]

minfree1 :: [Nat] -> Nat
minfree1 xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

checklist :: [Nat] -> Array Nat Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
              where n = length xs
