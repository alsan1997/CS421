{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Foldable
import qualified Data.List as List
import GHC.Generics (Generic)

import Lib

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup
      "mytake Function"
      [ testProperty
          "Behaves like standard library take"
          (prop_mytake_eq_take :: Small Int -> [Integer] -> Property)
      ]
  , testGroup
      "mydrop Function"
      [ testProperty
          "Behaves like standard library drop"
          (prop_mydrop_eq_drop :: Small Int -> [Integer] -> Property)
      ]
  , testGroup
      "rev Function"
      [ testProperty
          "Behaves like standard library reverse"
          (prop_rev_eq_reverse :: [Integer] -> Property)
      ]
  , testGroup
      "app Function"
      [ testProperty
          "Behaves like standard library (++)"
          (prop_app_eq_append :: [Integer] -> [Integer] -> Property)
      ]
  , testGroup
      "inclist Function"
      [ testProperty
          "Increments every element by 1"
          (prop_inclist_eq_mapPlus1 :: [Integer] -> Property)
      ]
  , testGroup
      "sumlist Function"
      [ testProperty
          "Behaves like standard library sum"
          (prop_sumlist_eq_sum :: [Integer] -> Property)
      ]
  , testGroup
      "myzip Function"
      [ testProperty
          "Behaves like standard library zip"
          (prop_myzip_eq_zip :: [Integer] -> [Integer] -> Property)
      ]
  , testGroup
      "addpairs Function"
      [ testProperty
          "Adds corresponding pairs of elements"
          (prop_addpairs_eq_zipWithPlus :: [Integer] -> [Integer] -> Property)
      ]
  , testGroup
      "ones Function"
      [testProperty "Is one at every index" prop_ones_iso_repeat1]
  , testGroup
      "nats Function"
      [testProperty "Enumerates the natural numbers" prop_nats_iso_enumFrom1]
  , testGroup
      "fib Function"
      [ testProperty "Is 0 at index 0" prop_fib_ix_0
      , testProperty "Is 1 at index 1" prop_fib_ix_1
      , testProperty
          "Implements the Fibonacci recurrence equation"
          prop_fib_ix_prevSum
      ]
  , testGroup
      "add Function"
      [ testProperty
          "Adds the provided element to the provided set"
          (prop_add_eq_sortNubCons :: Integer -> OrderedList Integer -> Property)
      ]
  , testGroup
      "union Function"
      [ testProperty
          "Behaves like standard library union, up to ordering"
          (prop_union_eq_sortListUnion :: OrderedList Integer -> OrderedList Integer -> Property)
      ]
  , testGroup
      "intersect Function"
      [ testProperty
          "Behaves like standard library intersect, up to ordering"
          (prop_intersect_eq_sortListIntersect :: OrderedList Integer -> OrderedList Integer -> Property)
      ] 
  , testGroup
      "powerset Function"
      [ testProperty
          "Behaves like standard library subsequences, up to ordering"
          (mapSize
             (`div` 5)
             (prop_powerset_eq_sortListSubsequences :: OrderedList Integer -> Property))
      ]
  , testGroup
      "inclist' Function"
      [ testProperty
          "Increments every element by 1"
          (prop_inclistP_eq_mapPlus1 :: [Integer] -> Property)
      ]
  , testGroup
      "sumlist' Function"
      [ testProperty
          "Behaves like standard library sum"
          (prop_sumlistP_eq_sum :: [Integer] -> Property)
      ]
  , testGroup
      "list2cons Function"
      [ testProperty
          "Converts [a] to (List a) exactly"
          (prop_list2cons_eq_foldableToCons :: [Integer] -> Property)
      ]
  , testGroup
      "cons2list Function"
      [ testProperty
          "Converts (List a) to [a] exactly"
          (prop_cons2list_eq_foldableToList :: List Integer -> Property)
      ]
  , testGroup
      "eval Function"
      [ testProperty
          "Evaluates expressions correctly"
          (mapSize (`div` 8) prop_eval_is_proper)
      ]
  , testGroup
      "list2cons' Function"
      [ testProperty
          "Converts [a] to (List a) exactly"
          (prop_list2consP_eq_foldableToCons :: [Integer] -> Property)
      ]
  , testGroup
      "sumTree Function"
      [ testProperty
          "Sums all values in the tree"
          (prop_sumTree_eq_foldableSum :: BinTree Integer -> Property)
      ]
  , testGroup
      "liftIntOp Function"
      [ testProperty
          "Lifts integer operations correctly"
          prop_liftIntOp_shows_properly
      ]
  ]

prop_mytake_eq_take :: (Eq a, Show a) => Small Int -> [a] -> Property
prop_mytake_eq_take (Small i) xs = mytake i xs === take i xs

prop_mydrop_eq_drop :: (Eq a, Show a) => Small Int -> [a] -> Property
prop_mydrop_eq_drop (Small i) xs = mydrop i xs === drop i xs

prop_rev_eq_reverse :: (Eq a, Show a) => [a] -> Property
prop_rev_eq_reverse xs = rev xs === reverse xs

prop_app_eq_append :: (Eq a, Show a) => [a] -> [a] -> Property
prop_app_eq_append xs ys = app xs ys === (xs ++ ys)

prop_inclist_eq_mapPlus1 :: (Eq a, Num a, Show a) => [a] -> Property
prop_inclist_eq_mapPlus1 xs = inclist xs === map (+ 1) xs

prop_sumlist_eq_sum :: (Eq a, Num a, Show a) => [a] -> Property
prop_sumlist_eq_sum xs = sumlist xs === sum xs

prop_myzip_eq_zip :: (Eq a, Eq b, Show a, Show b) => [a] -> [b] -> Property
prop_myzip_eq_zip xs ys = myzip xs ys === zip xs ys

prop_addpairs_eq_zipWithPlus :: (Eq a, Num a, Show a) => [a] -> [a] -> Property
prop_addpairs_eq_zipWithPlus xs ys = addpairs xs ys === zipWith (+) xs ys

prop_ones_iso_repeat1 :: NonNegative Int -> Property
prop_ones_iso_repeat1 (NonNegative i) = (ones !! i) === 1

prop_nats_iso_enumFrom1 :: NonNegative Int -> Property
prop_nats_iso_enumFrom1 (NonNegative i) = (nats !! i) === fromIntegral i

prop_fib_ix_0 :: Property
prop_fib_ix_0 = once $ (fib !! 0) === 0

prop_fib_ix_1 :: Property
prop_fib_ix_1 = once $ (fib !! 1) === 1

prop_fib_ix_prevSum :: NonNegative Int -> Property
prop_fib_ix_prevSum (NonNegative i) =
  (fib !! (i + 2)) === (fib !! i) + (fib !! (i + 1))

prop_add_eq_sortNubCons :: (Ord a, Show a) => a -> OrderedList a -> Property
prop_add_eq_sortNubCons x (Ordered xs) =
  add x xs' === List.sort (List.nub $ x : xs')
  where
    xs' = List.nub xs

prop_union_eq_sortListUnion ::
     (Ord a, Show a) => OrderedList a -> OrderedList a -> Property
prop_union_eq_sortListUnion (Ordered xs) (Ordered ys) =
  union xs' ys' === List.sort (List.union xs' ys')
  where
    xs' = List.nub xs
    ys' = List.nub ys

prop_intersect_eq_sortListIntersect ::
     (Ord a, Show a) => OrderedList a -> OrderedList a -> Property
prop_intersect_eq_sortListIntersect (Ordered xs) (Ordered ys) =
  intersect xs' ys' === List.sort (List.intersect xs' ys')
  where
    xs' = List.nub xs
    ys' = List.nub ys

prop_powerset_eq_sortListSubsequences ::
     (Ord a, Show a) => OrderedList a -> Property
prop_powerset_eq_sortListSubsequences (Ordered xs) =
  powerset xs' === List.sort (List.subsequences xs')
  where
    xs' = List.nub xs

prop_inclistP_eq_mapPlus1 :: (Eq a, Num a, Show a) => [a] -> Property
prop_inclistP_eq_mapPlus1 xs = inclist' xs === map (+ 1) xs

prop_sumlistP_eq_sum :: (Eq a, Num a, Show a) => [a] -> Property
prop_sumlistP_eq_sum xs = sumlist' xs === sum xs

prop_list2cons_eq_foldableToCons :: (Eq a, Show a) => [a] -> Property
prop_list2cons_eq_foldableToCons xs = list2cons xs === toCons xs

prop_cons2list_eq_foldableToList :: (Eq a, Show a) => List a -> Property
prop_cons2list_eq_foldableToList xz = cons2list xz === toList xz

prop_eval_is_proper :: Exp -> Property
prop_eval_is_proper xp = eval xp === evalProper xp
  where
    evalProper :: Exp -> Integer
    evalProper (IntExp n)    = n
    evalProper (PlusExp xps) = sum $ map evalProper xps
    evalProper (MultExp xps) = product $ map evalProper xps

prop_list2consP_eq_foldableToCons :: (Eq a, Show a) => [a] -> Property
prop_list2consP_eq_foldableToCons xs = list2cons' xs === toCons xs

prop_sumTree_eq_foldableSum :: (Eq a, Num a, Show a) => BinTree a -> Property
prop_sumTree_eq_foldableSum xs = sumTree xs === sum xs

prop_liftIntOp_shows_properly ::
     Fun (Integer, Integer) Integer -> SimpVal -> SimpVal -> Property
prop_liftIntOp_shows_properly f xv yv =
  show (liftIntOp f' xv yv) === show (liftIntOpProper f' xv yv)
  where
    liftIntOpProper ::
         (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
    liftIntOpProper f (IntVal i1) (IntVal i2) = IntVal $ f i1 i2
    liftIntOpProper _ _ _                     = ExnVal "not an IntVal!"
    f' = applyFun2 f

deriving instance Foldable List

toCons :: Foldable t => t a -> List a
toCons = foldr Cons Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap toCons (arbitrary :: Gen [a])
  shrink = map toCons . shrink . toList

deriving instance Generic Exp

instance Arbitrary Exp where
  arbitrary =
    oneof
      [ IntExp <$> arbitrary
      , PlusExp <$> scale (\s -> (s - 1) `max` 0) arbitrary
      , MultExp <$> scale (\s -> (s - 1) `max` 0) arbitrary
      ]
  shrink = genericShrink

deriving instance Foldable BinTree

deriving instance Generic (BinTree a)

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary =
    sized $ \s ->
      if s <= 0
        then return Leaf
        else oneof
               [ return Leaf
               , do n <- arbitrary
                    l <- scale (\s -> (s - 1) `max` 0) arbitrary
                    r <- scale (\s -> (s - 1) `max` 0) arbitrary
                    return $ Node n l r
               ]
  shrink = genericShrink

deriving instance Generic SimpVal

instance Arbitrary SimpVal where
  arbitrary =
    oneof
      [ IntVal <$> arbitrary
      , BoolVal <$> arbitrary
      , StrVal <$> arbitrary
      , return $ ExnVal "not an IntVal!"
      , ExnVal <$> arbitrary
      ]
  shrink = genericShrink
