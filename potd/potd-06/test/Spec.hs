import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.IntSet (IntSet, toList)
import Data.List

import Lib

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup
      "=G= Union"
      [ testProperty
          "=P= All elements in the two input sets are in the output (3 points)"
          preservesUnionInputs
      , testProperty
          "=P= All elements in the output are in one of the two input sets (3 points)"
          preservesUnionOutput
      ]
  , testGroup
      "=G= Intersection"
      [ testProperty
          "=P= Elements of input set A that are in the output must be in input set B, and vice versa (3 points)"
          preservesIntersectInputs
      , testProperty
          "=P= All elements in the output are in both of the two input sets (3 points)"
          preservesIntersectOutput
      ]
  , testGroup
      "=G= Cartesian Product"
      [ testProperty
          "=P= Cartesian product properly combines the two input sets (5 points)"
          preservesProduct
      ]
  ]

preservesUnionInputs :: IntSet -> IntSet -> Property
preservesUnionInputs xz yz =
  let xs = toList xz
      ys = toList yz
      res = xs \/ ys
  in xs `isSubsequenceOf` res .&&. ys `isSubsequenceOf` ys

preservesUnionOutput :: IntSet -> IntSet -> Property
preservesUnionOutput xz yz =
  let xs = toList xz
      ys = toList yz
      res = xs \/ ys
  in conjoin $ map (\elt -> elt `elem` xs .||. elt `elem` ys) res

preservesIntersectInputs :: IntSet -> IntSet -> Property
preservesIntersectInputs xz yz =
  let xs = toList xz
      ys = toList yz
      res = xs /\ ys
  in (xs `intersect` res) `isSubsequenceOf` ys .&&.
     (ys `intersect` res) `isSubsequenceOf` xs

preservesIntersectOutput :: IntSet -> IntSet -> Property
preservesIntersectOutput xz yz =
  let xs = toList xz
      ys = toList yz
      res = xs /\ ys
  in res `isSubsequenceOf` xs .&&. res `isSubsequenceOf` ys

preservesProduct :: IntSet -> IntSet -> Property
preservesProduct xz yz =
  let xs = toList xz
      ys = toList yz
      res = xs `x` ys
      xs_len = length xs
      ys_len = length ys
      res_len = xs_len * ys_len
      xs_product = replicate ys_len =<< xs
      ys_product = concat $ replicate xs_len ys
  in length res === res_len .&&.
     map fst res === xs_product .&&.
     map snd res === ys_product
