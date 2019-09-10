--
-- Implementation of Chapter 1 of "Pearls of Functional Algorithm Design"
--

module SmallestFreeNumber where

import Data.Array
import Data.Array.ST

type Nat = Int

testList :: [Nat]
testList = [08, 23, 09, 00, 12, 11 , 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 03, 06] :: [Nat]

checkIfAllPos :: (Integral a) => [a] -> ([a] -> b) -> Maybe b
checkIfAllPos xs f = if not $ all (>= 0) xs
                     then Nothing
                     else Just $ f xs

minfree1 :: [Nat] -> Maybe Nat
minfree1 xs = checkIfAllPos xs (\x -> head ([0..] \\ x))

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

checklist :: [Nat] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) (map fromIntegral xs)) (repeat True))
              where n = length xs

countlist :: [Nat] -> Array Nat Nat
countlist xs = accumArray (+) 0 (0, n) (zip (map fromIntegral xs) (repeat 1))
              where n = foldl max 0 xs -- 0 is identity element for max on natural numbers

minfreeArray :: [Nat] -> Maybe Nat
minfreeArray xs = checkIfAllPos xs (\x -> length $ takeWhile id (elems $ checklist x))

countsort :: [Nat] -> Maybe [Nat]
countsort [] = Just []
countsort xs = checkIfAllPos xs (\z -> concat [replicate (fromIntegral k) x | (x, k) <- assocs (countlist z)])

checklist_prosaic xs = runSTArray $ do
                                      a <- newArray (0, n) False
