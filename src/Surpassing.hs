module Surpassing where

import Data.List

msc :: (Ord a) => [a] -> Maybe Int
msc [] = Nothing
msc xs = Just $ maximum [scount z zs | z : zs <- myTails xs]

scount :: (Ord a) => a -> [a] -> Int
scount x xs = length $ filter (x <) xs

tcount :: (Ord a) => a -> [(a, Int)] -> Int
tcount z tys = length (dropWhile ((z >=) . fst) tys)

myTails :: [a] -> [[a]]
myTails [] = []
myTails (x:xs) = (x:xs) : myTails xs

table :: (Ord a) => [a] -> [(a, Int)]
table xs = sort [(z, scount z zs) | z:zs <- myTails xs]

mscTable :: (Ord a) => [a] -> Maybe Int
mscTable [] = Nothing
mscTable xs = Just . maximum . map snd . table $ xs

myJoin :: (Ord a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
myJoin txs tys = merge [(z, c + tcount z tys) | (z, c) <- txs] tys

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x  = x
merge x [] = x
merge (x:xs) (y:ys)
  | x < y     = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys
