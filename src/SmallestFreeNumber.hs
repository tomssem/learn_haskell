--
-- Implementation of Chapter 1 of "Pearls of Functional Algorithm Design"
--

module SmallestFreeNumber where

type Nat = Int

minfree1 :: [Nat] -> Nat
minfree1 xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us
