module SaddleBack where

invert :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invert f z = [(x, y) | x <- [0..z], y <- [0..z], f x y == z]

invertFaster :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invertFaster f z = [(x, y) | x <- [0..z - f 0 0], y <- [0..z - x - f 0 0], f x y == z]

find :: (Int, Int) -> (Int -> Int -> Int) -> Int -> [(Int, Int)]
find (u, v) f z = [(x, y) | x <- [u..z], y <- [v, v - 1 .. 0], f x y == z]

invertFancy :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invertFancy f z = find (0, z) f z
