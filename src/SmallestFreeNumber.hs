--
-- Implementation of Chapter 1 of "Pearls of Functional Algorithm Design"
--

module SmallestFreeNumber where

import Data.Array
import Data.Array.ST
import Data.List

type Nat = Int

testList :: [Nat]
testList = [08, 23, 09, 00, 12, 11 , 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 03, 06] :: [Nat]

allPos :: (Integral a) => [a] -> Bool
allPos = all (>= 0)

noDuplicates :: (Integral a) => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = (x `notElem` xs ) || noDuplicates xs

checkInvariants :: (Integral a) => [a] -> Bool
checkInvariants xs = all ($ xs) [noDuplicates, allPos]

guardInvariants :: (Integral a) => [a] -> ([a] -> b) -> Maybe b
guardInvariants xs f = if not $ checkInvariants xs
                     then Nothing
                     else Just $ f xs

minfree1 :: [Nat] -> Maybe Nat
minfree1 xs = guardInvariants xs (\x -> head ([0..] \\ x))

checklist :: [Nat] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) (map fromIntegral xs)) (repeat True))
              where n = length xs

countlist :: [Nat] -> Array Nat Nat
countlist xs = accumArray (+) 0 (0, n) (zip (map fromIntegral xs) (repeat 1))
              where n = foldl max 0 xs -- 0 is identity element for max on natural numbers

minfreeArray :: [Nat] -> Maybe Nat
minfreeArray xs = guardInvariants xs (\x -> length $ takeWhile id (elems $ checklist x))

countsort :: [Nat] -> Maybe [Nat]
countsort [] = Just []
countsort xs = guardInvariants xs (\z -> concat [replicate (fromIntegral k) x | (x, k) <- assocs (countlist z)])

checklistProsaic xs = runSTArray $ do
                                    a <- newArray (0, n) False
                                    sequence_ [writeArray a x True | x <- xs, x <= n]
                                    return a
                            where n = length xs

minfreeDivConq :: [Nat] -> Maybe Nat
minfreeDivConq xs = guardInvariants xs (\x -> minfrom 0 (length x, x))

minfrom :: Nat -> (Int, [Nat]) -> Nat
minfrom a (n, xs) | null xs            = a
                  | m == b - a         = minfrom b (n - m, vs)
                  | otherwise          = minfrom a (m, us)
                    where (us, vs) = partition (< b) xs
                          b        = a + 1 + n `div` 2
                          m        = length us
