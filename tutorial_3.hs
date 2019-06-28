import Data.List
import System.IO 

factorial :: Int -> Int
isOdd :: Int -> Bool 
--isEven :: Int -> Bool

whatGrade :: Int -> String
batAvgRating :: Double -> Double -> String
getListItems :: [Int] -> String
getFirstItem :: String -> String

-- function declaration

factorial 0 = 1
factorial n = n * factorial (n - 1) 

-- 3 * factorial(3-1 = 2)
-- 2 * factorial(2-1 = 1) 
-- 1 * factorial(1-1 = 0) : 1 (and then we move upward and replace each value)

--FIBONNACI
--1,1,2,3,5,8,13....
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

-- 1st : fib = 1 and (Tail fib) = 1
-- [1, 1, 2] : a: 1 + b: 1 = 2

-- 2nd : fib = 1 and (tail fib) = 2
-- [1, 1, 2, 3]: a: 1 + b: 2 = 3

fib20 = fib !! 19 -- the 20th fib
showFib20 = take 20 fib 

prodFact n = product [1..n]

isOdd n
 | n `mod` 2 == 0 = False
 | otherwise = True

isEven n = n `mod` 2 == 0

whatGrade age 
 | (age >= 5) && (age <= 6) = "Kindergarden"
 | (age > 6) && (age <= 10) = "Elementary school"
 | (age > 10) && (age <= 14) = "Middle school"
 | (age > 14) && (age <= 18) = "High school"
 | otherwise = "Go to college"

batAvgRating hits atBats
 | avg <= 0.200 = "Terrible"
 | avg <= 0.250 = "Average"
 | avg <= 0.280 = "You're good"
 | otherwise = "You're a superstar"
 where avg = hits / atBats

getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest of the item are " ++ show xs 

getFirstItem [] = "empty string"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x] 