import Control.Monad.State  
import Control.Monad.Writer
import Data.List  
import Data.Ratio  

type Stack = [Int] 
      
pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

f = runState (join (state $ \s -> (push 10,1:2:s))) [0,0,0]

keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing

type KnightPos = (Int,Int)  

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  

in3 :: KnightPos -> [KnightPos]  
in3 start = moveKnight start >>= moveKnight >>= moveKnight  

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  

  
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x = foldr (<=<) return (replicate x moveKnight)

g :: (Monad m) => m Int -> m Int
g = foldr (<=<) return [(+1),(*100),(+1)]

newtype Prob a = Prob { getProb :: (a, Rational)} deriving Show
