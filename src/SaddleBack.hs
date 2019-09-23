module SaddleBack where

import Debug.Trace

invert :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invert f z = [(x, y) | x <- [0..z], y <- [0..z], f x y == z]

invertFaster :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invertFaster f z = [(x, y) | x <- [0..z - f 0 0], y <- [0..z - x - f 0 0], f x y == z]

find :: (Int, Int) -> (Int -> Int -> Int) -> Int -> [(Int, Int)]
find (u, v) f z
  | u > z || v < 0 = []
  | z' < z         = find (u+1, v) f z
  | z' == z        = (u, v) : find (u + 1, v - 1) f z
  | z' > z         = find (u, v-1) f z
    where z' = f u v

-- [(x, y) | x <- [u..z], y <- [v, v - 1 .. 0], f x y == z]

invertFancy :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invertFancy f z = find (m, n) f z
                    where m = maximum (filter (\y -> f 0 y <= z) [0..z]) -- bsearch (\y -> f 0 y) (-1, z+1) z
                          n = z -- bsearch (\x -> f x 0) (-1, z+1) z

bsearch g (a, b) z
  | a+1 == b = a
  | g m <= z = bsearch g (m, b) z
  | otherwise = bsearch g (a, m) z
    where m = (a + b) `div` 2

