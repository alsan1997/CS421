module Spec where

import Test.Framework ( defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Tests

main :: IO()
main = defaultMain tests

tests = [
    testGroup "=G= Unit Tests for Unify"
    (zipWith convert_test [1..] (test_group_unify))
  , testGroup "=G= Unit Tests for Infer"
    (zipWith convert_test [(length test_group_unify+1)..] (test_group_infer))
  ]

test_group_unify = concat [
      tests_unify_elim
    , tests_unify_del
    , tests_unify_orient
    , tests_unify_decomp
    , tests_unify_error
    , tests_unify_comp
    ]

test_group_infer = concat [
      tests_infer_const
    , tests_infer_var
    , tests_infer_let
    , tests_infer_op
    , tests_infer_cond
    , tests_infer_fun
    , tests_infer_app
    , tests_infer_rec
    ]

convert_test :: Int -> (String, String, Int) -> Test
convert_test n (real, target, score)
    = testProperty ("=P= Unit Test #" ++ show n ++ " (" ++ show score ++ " points)") (once $ real === target)
