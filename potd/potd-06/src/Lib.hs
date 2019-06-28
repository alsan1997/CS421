module Lib
    ( (\/)
    , (/\)
    , x
    ) where

-- Your code here!

(\/) :: Ord a => [a] -> [a] -> [a]
(\/) [] [] = []
(\/) xx [] = xx
(\/) [] yy = yy
(\/) (x:xs) (y:ys) | x == y = x : (\/) xs ys
                   | x < y = x : (\/) xs (y:ys)
                   | otherwise = y : (\/) (x:xs) ys

(/\) :: Ord a => [a] -> [a] -> [a]
(/\) [] [] = []
(/\) xx [] = []
(/\) [] yy = []
(/\) (x:xs) (y:ys) | x < y = (/\) xs (y:ys)
                   | x == y = x : (/\) xs ys
                   | otherwise = (/\) (x:xs) ys

x :: [a] -> [b] -> [(a, b)]
x [] [] = []
x [] yy = []
x xx [] = []
x xx yy = [(x,y) | x <- xx, y <- yy]
 

