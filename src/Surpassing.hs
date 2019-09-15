module Surpassing where

msc :: (Ord a) => [a] -> Maybe Int
msc [] = Nothing
msc xs = Just $ maximum [scount z zs | z : zs <- myTails xs]

scount :: (Ord a) => a -> [a] -> Int
scount x xs = length $ filter (x <) xs

myTails :: [a] -> [[a]]
myTails [] = []
myTails (x:xs) = (x:xs) : myTails xs
