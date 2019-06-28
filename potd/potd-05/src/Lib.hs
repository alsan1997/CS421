module Lib
    ( split
    , merge
    , mergesort
    ) where

-- Your code here!

split :: Ord a => [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:xys) = (x:xs, y:ys) 
    where (xs, ys) = split xys


merge :: Ord a => [a] -> [a] -> [a]
merge [] xx = xx
merge xx [] = xx
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xx = let (list1, list2) = split xx in merge (mergesort(list1)) (mergesort(list2))