module Surpassing where

msc :: (Ord a) => [a] -> Maybe Int
msc [] = Nothing
msc xs = Just $ length xs

myTails :: [a] -> [[a]]
myTails [] = []
myTails (x:xs) = (x:xs) : myTails xs
