module Lib
    ( toDigits
    , doubleEveryOther
    ) where

-- Your code here!
-- Remember that you are always free to write helper functions.

toDigits :: Integer -> [Integer]
toDigits i | i == 0 = [] 
           | otherwise = toDigits (i `div` 10) ++ [i `mod` 10]

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xx@(x:y:xs) | (length xx) `mod` 2 == 0 = x*2 : y : doubleEveryOther(xs)
                             | otherwise = x : y*2 : doubleEveryOther(xs)
