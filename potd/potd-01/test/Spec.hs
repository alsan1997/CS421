import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [ testGroup "SumNaturals Function" [
                testProperty "Value is positive" (forAll randInputs positiveValue)
              ]
      ]

randInputs = choose (0, 10)

positiveValue :: Int -> Bool
positiveValue x = sumNaturals x > 0

