{-# LANGUAGE TupleSections, ViewPatterns #-}
module Spec where

import Test.Framework ( defaultMainWithOpts, testGroup, TestOptions, RunnerOptions
                      , topt_maximum_generated_tests, ropt_test_options, topt_timeout
                      )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.ParserCombinators.Parsec (parse)
import qualified Data.HashMap.Strict as H
import Data.List (isInfixOf, intercalate, intersperse, nub)
import Data.Either (either)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Traversable

import Main hiding (main)
import Scheme.Core
import Scheme.Parse
import Scheme.Eval
import Scheme.Runtime

import Tests

main :: IO ()    
main = defaultMainWithOpts tests my_runner_opts
     where my_test_opts = (mempty :: TestOptions) { topt_maximum_generated_tests = Just 1000, topt_timeout = Just (Just 5000000) }
           my_runner_opts = (mempty :: RunnerOptions) { ropt_test_options = Just my_test_opts }

-- | Weird version of foldl required by Scheme semantics.
foldlS :: Foldable t => (a -> a -> a) -> a -> t a -> a
foldlS f z xs | length xs <= 1  = foldl f z xs
              | otherwise = foldl1 f xs

convLoose2Bool :: Val -> Bool
convLoose2Bool (flattenList -> Boolean b) = b
convLoose2Bool _ = True

quoteSafeShow :: Val -> String
quoteSafeShow (List (Symbol "lambda" : List ps: bs)) = "(" ++ intercalate " " (map show [Symbol "lambda", List ps] ++ map quoteSafeShow bs) ++ ")"
quoteSafeShow l@(List _) = '\'' : show l
quoteSafeShow l@(DottedList _ _) = '\'' : show l
quoteSafeShow s@(Symbol _) = '\'' : show s
quoteSafeShow v = show v

identString = oneof [((:) <$> elements initChars <*> listOf (elements restChars)),
                     ((\c cs -> '-':c:cs) <$> elements initChars' <*> listOf (elements restChars))]
  where initChars = "*+/:?><=!" ++ ['a'..'z'] ++ ['A'..'Z']
        initChars' = '-' : initChars
        restChars = initChars' ++ ['0'..'9']

instance Arbitrary Val where
  arbitrary = flattenList <$> oneof [arbSymbol, arbBoolean, arbNumber, arbList, arbDottedList, arbLambda]
    where arbSymbol = Symbol <$> identString
          arbBoolean = Boolean <$> arbitrary
          arbNumber = Number <$> arbitrary
          arbList = List <$> scale (max 0 . subtract 1) arbitrary
          arbDottedList = DottedList <$> scale (max 0 . subtract 1) arbitrary <*> scale (max 0 . subtract 1) arbitrary
          arbLambda = (\vars body -> List [Symbol "lambda", List vars, body]) <$> listOf arbSymbol <*> scale (max 0 . subtract 1) arbitrary
  shrink (Boolean b) = [ Boolean b' | b' <- shrink b ]
  shrink (Number n) = [Boolean True, Boolean False] ++ [ Number n' | n' <- shrink n ]
  shrink (List (v:vs)) = [Boolean True, Boolean False, List []] ++ (v:vs)
  shrink (DottedList vs v) = [Boolean True, Boolean False, List []] ++ vs ++ [v]
  shrink Void = []
  shrink _ = [Boolean True, Boolean False]

tests =
  [ testGroup "Arithmetic Operators"
    [ testProperty "`+` adds numbers" sum_arith_prop
    , testProperty "`-` subtracts numbers" diff_arith_prop
    , testProperty "`*` multiplies numbers" prod_arith_prop
    , testProperty "`/` divides numbers" div_arith_prop
    ]
  , testGroup "Boolean Operators"
    [ testProperty "`and` conjoins booleans" and_strict_bool_prop
    , testProperty "`and` handles non-booleans properly" (mapSize (`div` 9) and_loose_bool_prop)
    , testProperty "`or` disjoins booleans" or_strict_bool_prop
    , testProperty "`or` handles non-booleans properly" (mapSize (`div` 9) or_loose_bool_prop)
    ]
  , testGroup "Comparison Operators"
    [ testProperty "`<` compares numbers for strict lesserness" lt_comp_prop
    , testProperty "`<=` compares numbers for loose lesserness" le_comp_prop
    , testProperty "`<` compares numbers for strict greaterness" gt_comp_prop
    , testProperty "`>=` compares numbers for loose greaterness" ge_comp_prop
    ]
  , testGroup "List Operations"
    [ testProperty "`car` returns the first element of a proper list" car_is_head_1_prop
    , testProperty "`car` returns the first element of a dotted list" car_is_head_2_prop
    , testProperty "`car` errors on empty lists" car_is_head_3_prop
    , testProperty "`cdr` returns the remaining elements of a proper list" car_is_head_1_prop
    , testProperty "`cdr` returns the remaining elements of a dotted list" car_is_head_2_prop
    , testProperty "`cdr` errors on empty lists" car_is_head_3_prop
    , testProperty "`list` makes its arguments into a proper list" list_makes_lists_prop
    , testProperty "`cons` sticks its first argument in the front of its second argument" cons_is_cons_prop
    ]
  , testGroup "Unary Operators"
    []
  , testGroup "= and eq? Operators"
    [ testProperty "`=` compares numbers for equality exactly" eq_exact_num_prop
    , testProperty "`=` compares booleans for equality exactly" eq_exact_bool_prop
    , testProperty "`eq?` compares numbers, booleans or symbols for equality loosely" (forAll (listOf inexact_gen) eq_inexact_prop)
    ]
  , testGroup "modulo"
    [ testProperty "`modulo` takes the modulo of two numbers" mod_arith_prop ]
  , testGroup "define Special Form"
    [ testProperty "`define` has no output when successful" (mapSize (`div` 7) $ forAll identString define_has_no_output_prop)
    , testProperty "`define` defines new names" (mapSize (`div` 7) $ forAll identString define_defines_prop)
    ]
  , testGroup "Individual Unit Tests"
    (zipWith (\n p -> testProperty ("Unit Test #" ++ show n) (once p)) [1..] (tests_runtime ++ tests_evaluator))
  ]

sum_arith_prop :: [Int] -> Property
sum_arith_prop ns = withRuntime ["(+" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow (Number $ sum ns)]

diff_arith_prop :: [Int] -> Property
diff_arith_prop ns = withRuntime ["(-" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow (Number $ foldlS (-) 0 ns)]

prod_arith_prop :: [Int] -> Property
prod_arith_prop ns = withRuntime ["(*" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow (Number $ product ns)]

div_arith_prop :: [Int] -> Property
div_arith_prop ns = check ns ==> withRuntime ["(/" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow (Number $ foldlS div 1 ns)]
  where check [] = True
        check [n] = n /= 0
        check ns = all (/= 0) $ tail ns

and_strict_bool_prop :: [Bool] -> Property
and_strict_bool_prop bs = withRuntime ["(and" ++ (concatMap ((" " ++ ) . quoteSafeShow . Boolean) bs) ++ ")"] === [quoteSafeShow (Boolean $ and bs)]

and_loose_bool_prop :: [Val] -> Property
and_loose_bool_prop vs = withRuntime ["(and" ++ (concatMap ((" " ++ ) . quoteSafeShow) vs) ++ ")"] === [quoteSafeShow (Boolean $ all convLoose2Bool vs)]

or_strict_bool_prop :: [Bool] -> Property
or_strict_bool_prop bs = withRuntime ["(or" ++ (concatMap ((" " ++ ) . quoteSafeShow . Boolean) bs) ++ ")"] === [quoteSafeShow (Boolean $ or bs)]

or_loose_bool_prop :: [Val] -> Property
or_loose_bool_prop vs = withRuntime ["(or" ++ (concatMap ((" " ++ ) . quoteSafeShow) vs) ++ ")"] === [quoteSafeShow (Boolean $ any convLoose2Bool vs)]

lt_comp_prop :: [Int] -> Property
lt_comp_prop ns = withRuntime ["(<" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (<) ns (drop 1 ns)]

le_comp_prop :: [Int] -> Property
le_comp_prop ns = withRuntime ["(<=" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (<=) ns (drop 1 ns)]

gt_comp_prop :: [Int] -> Property
gt_comp_prop ns = withRuntime ["(>" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (>) ns (drop 1 ns)]

ge_comp_prop :: [Int] -> Property
ge_comp_prop ns = withRuntime ["(>=" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (>=) ns (drop 1 ns)]

car_is_head_1_prop :: [Int] -> Property
car_is_head_1_prop vs = not (null vs) ==> withRuntime ["(car '(" ++ intercalate " " (map show vs) ++ "))"] === [show . Number $ head vs]

car_is_head_2_prop :: [Int] -> Int -> Property
car_is_head_2_prop vs v = not (null vs) ==> withRuntime ["(car '(" ++ intercalate " " (map show vs ++ [".", show v]) ++ "))"] === [show . Number $ head (vs ++ [v])]

car_is_head_3_prop :: Property
car_is_head_3_prop = once $ withRuntime ["(car '())"] === ["Error: Unexpected arguments or wrong number of arguments (())"]

cdr_is_tail_1_prop :: [Int] -> Property
cdr_is_tail_1_prop vs = not (null vs) ==> withRuntime ["(cdr '(" ++ intercalate " " (map show vs) ++ "))"] === [show . List . map Number $ tail vs]

cdr_is_tail_2_prop :: [Int] -> Int -> Property
cdr_is_tail_2_prop vs v = not (null vs) ==> withRuntime ["(cdr '(" ++ intercalate " " (map show vs ++ [".", show v]) ++ "))"] === [show . List . map Number $ tail (vs ++ [v])]

cdr_is_tail_3_prop :: Property
cdr_is_tail_3_prop = once $ withRuntime ["(cdr '())"] === ["Error: Unexpected arguments or wrong number of arguments (())"]

list_makes_lists_prop :: [Int] -> Property
list_makes_lists_prop vs = withRuntime ["(list" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) vs) ++ ")"] === [show (List $ map Number vs)]

cons_is_cons_prop :: Int -> [Int] -> Property
cons_is_cons_prop v vs = withRuntime ["(cons " ++ show v ++ " '(" ++ intercalate " " (map show vs) ++ "))"] === [show (List $ map Number (v:vs))]

eq_exact_num_prop :: [Int] -> Property
eq_exact_num_prop ns = withRuntime ["(=" ++ (concatMap ((" " ++ ) . quoteSafeShow . Number) ns) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (==) ns (drop 1 ns)]

eq_exact_bool_prop :: [Bool] -> Property
eq_exact_bool_prop bs = withRuntime ["(=" ++ (concatMap ((" " ++ ) . quoteSafeShow . Boolean) bs) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (==) bs (drop 1 bs)]

inexact_gen :: Gen (Either Int (Either Bool String))
inexact_gen = oneof [Left <$> arbitrary, Right . Left <$> arbitrary, Right . Right <$> identString]

eq_inexact_prop :: [Either Int (Either Bool String)] -> Property
eq_inexact_prop vs = withRuntime ["(eq?" ++ (concatMap ((" " ++ ) . quoteSafeShow . either Number (either Boolean Symbol)) vs) ++ ")"] === [quoteSafeShow . Boolean . and $ zipWith (==) vs (drop 1 vs)]

mod_arith_prop :: Int -> Int -> Property
mod_arith_prop n m = m /= 0 ==> withRuntime ["(modulo " ++ show n ++ " " ++ show m ++ ")"] === [quoteSafeShow (Number $ n `mod` m)]

define_has_no_output_prop :: String -> Val -> Property
define_has_no_output_prop name body = withRuntime ["(define " ++ name ++ " " ++ quoteSafeShow body ++ ")"] === [""]

define_defines_prop :: String -> Val -> Property
define_defines_prop name body = check body ==> withRuntime ["(define " ++ name ++ " " ++ quoteSafeShow body ++ ")", name] === ["", show body]
  where check (flattenList -> List (Symbol "lambda":_)) = False
        check _ = True
