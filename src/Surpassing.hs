module Surpassing where

msc :: (Ord a) => [a] -> Maybe Int
msc [] = Nothing
msc xs = Just $ maximum [scount z zs | z : zs <- myTails xs]

scount :: (Ord a) => a -> [a] -> Int
scount x xs = length $ filter (x <) xs

tcount :: (Ord a) => a -> [(a, Int)] -> Int
tcount z tys = scount z (map fst tys)

myTails :: [a] -> [[a]]
myTails [] = []
myTails (x:xs) = (x:xs) : myTails xs

table :: (Ord a) => [a] -> [(a, Int)]
table xs = [(z, scount z zs) | z:zs <- myTails xs]

mscTable :: (Ord a) => [a] -> Maybe Int
mscTable [] = Nothing
mscTable xs = Just . maximum . map snd . table $ xs

join :: (Ord a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join txs tys = [(z, c + tcount z tys) | (z, c) <- txs] ++ tys
