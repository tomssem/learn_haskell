module Main where

import System.Environment

import Haq

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . haqify . head

