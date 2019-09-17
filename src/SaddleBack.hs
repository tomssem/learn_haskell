module SaddleBack where

invert :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
invert f z = [(x, y) | x <- [0..z], y <- [0..z], f x y == z]
