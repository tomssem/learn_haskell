module Test where

import Control.Monad

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust $ f x


instance Applicative MyMaybe where
  pure = MyJust
  MyNothing <*> _ = MyNothing
  _ <*> MyNothing = MyNothing
  MyJust f <*> x = fmap f x

instance Monad MyMaybe where
  return = MyJust
  MyNothing >>= _ = MyNothing
  MyJust x >>= f = f x
  fail _ = MyNothing

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) 
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    return (x > 8)  

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do 
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = moveKnight start >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
