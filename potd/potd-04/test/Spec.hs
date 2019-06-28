import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [
          testGroup "Sum Digits" [
                testProperty "Output is correct for lists with single digit numbers (2 points)" (forAll randSingleList correctlySums)
              , testProperty "Output is correct for lists with double digit numbers (3 points)" (forAll randDoubleList correctlySums)
           ]
        , testGroup "Validate" [
                testProperty "Output is correct (5 points)" (forAll randInteger correctlyValidates)
           ]
      ]

randDigit :: Gen (Integer, Integer)
randDigit = do d <- choose (0,9)
               return (d,d)

randDouble :: Gen (Integer, Integer)
randDouble = do d1 <- choose (1,9)
                d2 <- choose (0,9)
                return (d1 * 10 + d2,d1 + d2)

dsum :: Integer -> Integer
dsum 0 = 0
dsum 1 = 2
dsum 2 = 4
dsum 3 = 6
dsum 4 = 8
dsum 5 = 1
dsum 6 = 3
dsum 7 = 5
dsum 8 = 7
dsum 9 = 9

randListPart :: Gen ([Integer], Integer)
randListPart = do n1 <- choose (0,9)
                  n2 <- choose (0,9)
                  return ([n1,n2],n1 + dsum n2)

randInteger :: Gen (Integer,Integer)
randInteger = do l <- listOf randListPart
                 return (digitsToInteger (reverse (concatMap fst l)), sum (map snd l))

emptyList :: Gen [a]
emptyList = return []

randSingleList = listOf randDigit
randDoubleList = listOf randDouble

digitsToInteger :: [Integer] -> Integer
digitsToInteger digits = foldl (\a b -> a * 10 + b) 0 digits

correctResult :: Integer -> Bool
correctResult x = digitsToInteger (toDigits x) == x

isDoubled []  [] = True
isDoubled [x] [y]  = x == y
isDoubled (x1:x2:xs) (y1:y2:ys) = x1 == y1 && x2 * 2 == y2 && isDoubled xs ys

correctlySums :: [(Integer,Integer)] -> Bool
correctlySums a = sumDigits (map fst a) == sum (map snd a)

correctlyValidates :: (Integer,Integer) -> Bool
correctlyValidates (i,s) = s `mod` 10 == 0 && validate i || not (validate i)


