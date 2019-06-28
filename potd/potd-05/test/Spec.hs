import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Mergesort Function" [
                testProperty "Length is preserved" (forAll randList sameLength)
                -- Add your own tests here!
           ]
      ]

randDigit :: Gen Integer
randDigit = choose (0,20)

randList = listOf randDigit

sameLength :: (Ord a) => [a] -> Bool
sameLength x = length x == (length $ mergesort x)
