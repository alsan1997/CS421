import Data.List
import System.IO 

decList (x:xs) = x - 1 : decList(xs)
decList [] = []

rev [] = []
rev (x:xs) = (rev xs) ++ [x]

sumList [] a = a
sumList (x:xs) a = sumList xs (a + x)

incList [] a = reverse a
incList (x:xs) a = incList xs (x + 1 : a)


chop :: [Int] -> [Int]
 
chop xx = aux xx (length xx) 
 where aux [] len = []
       aux [x] len | x > 0 = replicate (length xx - len) 0 ++ [x - 1] | otherwise = replicate (length xx) 0
       aux (x:y:xs) len | x > 0 = replicate (length xx - len) 0 ++ (x - 1:y + (len -  1) : xs) | otherwise = aux (y:xs) (len - 1)


many3s = replicate 5 3 ++ [3+1,4,5]
lenx xx = length xx

fact :: Int -> Int

fact n = aux n 1
 where aux 0 a = a
       aux n a = aux (n-1) (n*a)

maxList [x] = x
maxList (x:xs) = max x (maxList xs)

maxi :: [Int] -> Int

maxi [x] = x
maxi (x:xs) | (maxi xs) > x = maxi (xs) | otherwise = x

tupLen xx = length xx