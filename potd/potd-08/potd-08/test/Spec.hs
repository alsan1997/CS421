module Spec ( main ) where
import Prelude hiding (id, (.))
import Control.Category

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad.Trans.State.Lazy (State)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import Lib

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup
      "`get`"
      [ testProperty
          "`get` behaves properly on a simple lens."
          prop_get_basic_behavior
      , testProperty
          "`get` behaves properly on a complex lens."
          prop_get_advanced_behavior
      ]
  , testGroup
      "set"
      [ testProperty
          "`set` behaves properly on a simple lens."
          prop_set_basic_behavior
      , testProperty
          "`set` behaves properly on a complex lens."
          prop_set_advanced_behavior
      ]
  , testGroup
      "`makeLens`"
      [ testProperty
          "The getter you gave to `makeLens` is what you get back with `get`."
          prop_makeLens_recovers_get
      , testProperty
          "The setter you gave to `makeLens` is what you get back with `set`."
          prop_makeLens_recovers_set
      , testProperty
          "Using `makeLens` on an existing simple lens’s `set` and `get` functions gives you back the same lens."
          prop_makeLens_splits_basic
      , testProperty
          "Using `makeLens` on an existing complex lens’s `set` and `get` functions gives you back the same lens."
          prop_makeLens_splits_advanced
      ]
  ]

-- Setup
testLensA :: Lens (Integer, String, IntSet) IntSet
testLensA = Lens $ \(n, s, nz) -> (nz, \nz' -> (n, s, nz'))

testLensB :: Int -> Lens IntSet Bool
testLensB m =
  Lens $ \nz ->
    ( IS.member m nz
    , \p ->
        (if p
           then IS.insert
           else IS.delete)
          m
          nz)

testLensBoA :: Int -> Lens (Integer, String, IntSet) Bool
testLensBoA m =
  Lens $ \(n, s, nz) ->
    ( IS.member m nz
    , \p ->
        if p
          then (n, s, IS.insert m nz)
          else (n, s, IS.delete m nz))

-- Tests
prop_get_basic_behavior :: (Integer, String, IntSet) -> Property
prop_get_basic_behavior v@(_, _, nz) = get testLensA v === nz

prop_get_advanced_behavior :: Int -> IntSet -> Property
prop_get_advanced_behavior m nz = get (testLensB m) nz === IS.member m nz

prop_set_basic_behavior :: (Integer, String, IntSet) -> IntSet -> Property
prop_set_basic_behavior v@(n, s, nz) nz' = set testLensA v nz' === (n, s, nz')

prop_set_advanced_behavior :: Int -> IntSet -> Bool -> Property
prop_set_advanced_behavior m nz p = set (testLensB m) nz p === op nz
  where
    op =
      (if p
         then IS.insert
         else IS.delete)
        m

prop_makeLens_recovers_get ::
     Fun Integer String -> Fun (Integer, String) Integer -> Integer -> Property
prop_makeLens_recovers_get (Fn getter) (Fn2 setter) n = get l n === getter n
  where
    l = makeLens getter setter

prop_makeLens_recovers_set ::
     Fun Integer String
  -> Fun (Integer, String) Integer
  -> Integer
  -> String
  -> Property
prop_makeLens_recovers_set (Fn getter) (Fn2 setter) n s =
  set l n s === setter n s
  where
    l = makeLens getter setter

prop_makeLens_splits_basic :: (Integer, String, IntSet) -> IntSet -> Property
prop_makeLens_splits_basic v nz'' = nz' === nz .&&. sf' nz'' === sf nz''
  where
    Lens l = testLensA
    Lens l' = makeLens (get testLensA) (set testLensA)
    (nz, sf) = l v
    (nz', sf') = l' v

prop_makeLens_splits_advanced :: Int -> IntSet -> Bool -> Property
prop_makeLens_splits_advanced m nz p'' = p' === p .&&. sf' p'' === sf p''
  where
    l0 = testLensB m
    Lens l = l0
    Lens l' = makeLens (get l0) (set l0)
    (p, sf) = l nz
    (p', sf') = l' nz
