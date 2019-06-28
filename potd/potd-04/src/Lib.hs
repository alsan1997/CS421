module Lib
    ( sumDigits
    , validate
    , toDigits
    , doubleEveryOther
    ) where

toDigits :: Integer -> [Integer]
toDigits n = aux n []
  where aux 0 result = result
        aux n result = let (d, r) = divMod n 10
                       in aux d (r:result)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ aux (reverse xs)
  where aux []       = []
        aux [x]      = [x]
        aux (x:y:xs) = x : 2*y : aux xs

-- Your code here!

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) | x > 9 = sumDigits(toDigits(x)) + sumDigits(xs)
                 | otherwise = x + sumDigits(xs)

validate :: Integer -> Bool
validate i | sumDigits(doubleEveryOther(toDigits(i))) `mod` 10 == 0 = True 
           | otherwise = False
