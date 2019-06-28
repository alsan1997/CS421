import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [
          testGroup "To Digits Function" [
                testProperty "Output is correct for positive integers" (forAll posInputs correctResult)
              , testProperty "Output is correct for 0" (forAll zeroInput correctResult)
           ]
        , testGroup "Double Every Other Function" [
                testProperty "Output is correct for non-empty lists" (forAll randList correctlyDoubles)
              , testProperty "Output is correct for empty list" (forAll emptyList correctlyDoubles)
           ]
      ]

posInputs = choose (1, 20000)
zeroInput = return 0

randDigit :: Gen Integer
randDigit = choose (0,9)

emptyList :: Gen [a]
emptyList = return []

randList = listOf randDigit

digitsToInteger :: [Integer] -> Integer
digitsToInteger digits = foldl (\a b -> a * 10 + b) 0 digits

correctResult :: Integer -> Bool
correctResult x = digitsToInteger (toDigits x) == x

isDoubled []  [] = True
isDoubled [x] [y]  = x == y
isDoubled (x1:x2:xs) (y1:y2:ys) = x1 == y1 && x2 * 2 == y2 && isDoubled xs ys

correctlyDoubles :: [Integer] -> Bool
correctlyDoubles a = isDoubled (reverse a) (reverse $ doubleEveryOther a)
