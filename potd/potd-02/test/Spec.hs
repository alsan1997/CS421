import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Prelude hiding (signum)

import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Signum Function" [
                testProperty "Output is correct for positive inputs" (forAll posInputs signumReturnsPos)
           ]
      , testGroup "If' Function" [
                testProperty "Output is correct for true inputs"  (forAll trueInputs ifCorrectValue)
           ]
      ]

posInputs = choose (1, 100)

trueInputs = do
    thenBit <- choose (1,100)
    elseBit <- choose (1,100)
    return (True,thenBit,elseBit)

signumReturnsPos :: Int -> Bool
signumReturnsPos x = signum x == 1


ifCorrectValue :: (Bool,Int,Int) -> Bool
ifCorrectValue (c,t,e) | c          = (if' c t e) == t
                       | otherwise  = (if' c t e) == e

