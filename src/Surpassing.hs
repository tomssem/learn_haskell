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
table [] = []
table [x] = [(x, 0)]
table xs = myJoin (m - n) (table ys) (table zs)
           where m = length xs
                 n = m `div` 2
                 (ys, zs) = splitAt n xs

mscTable :: (Ord a) => [a] -> Maybe Int
mscTable [] = Nothing
mscTable xs = Just . maximum . map snd . table $ xs

myJoin :: (Ord a) => Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
myJoin 0 txs [] = txs
myJoin _ [] tys = tys
myJoin n txs@((x, c) : txs') tys@((y, d) : tys') 
  | x < y     = (x, c + n) : myJoin n txs' tys
  | otherwise = (y, d) : myJoin (n - 1) txs tys'

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] x  = x
merge x [] = x
merge (x:xs) (y:ys)
  | x < y     = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys
