import Data.List
import System.IO 
import Language.Haskell.TH

primeNumbers = [3,5,7,11]

morePrime = primeNumbers ++ [13,17,19,23,29]

favNums = 2:7:21:66:[]

morePrime2 = 2 : morePrime

lenPrime = length morePrime2
revPrime = reverse morePrime2
isListEmpty = null morePrime2
secondPrime = morePrime !! 1

firstPrime = head morePrime2
lastPrime = last morePrime2

first3Primes = take 3 morePrime2

is7InList = 7 `elem` morePrime2

evenList = [0,2..20]


many2s = take 5 (repeat 2)
many3s = replicate 5 3
cycleList = take 10 (cycle [1,2,3])

listTimes2 = [x*2 | x <- [1..10], x * 3 <= 10]

divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,1,8,3,4,7,6]
sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]
sumLists = sum [1,2,3,4,5]

listBiggerThan5 = filter (>5) sortedList

evensUpTo20 = takeWhile (<= 20) [2,4..]

multOfList = foldl (*) 2 [2,3,4,5]

pow3List = [3^n | n <- [1..10]]

multTable = [[x*y | y <- [1..10]] | x <- [1..10]]
--TUPLES
randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52)

bobsname = fst bobSmith
bobsage = snd bobSmith

names = ["Bob", "Mary", "Tom"]
addresses = ["123 Main", "234 North", "567 South"]

namesNAddress = zip names addresses

maxList (x:xs) = foldr(\x y-> if x > y then x else y) 0 (x:xs)
