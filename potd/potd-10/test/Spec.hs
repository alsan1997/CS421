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
      "`modify`"
      [ testProperty
          "`modify` behaves properly on a simple lens."
          prop_modify_basic_behavior
      , testProperty
          "`modify` behaves properly on a complex lens."
          prop_modify_advanced_behavior
      ]
  , testGroup
      "`modifyMap`"
      [ testProperty
          "`modifyMap` behaves properly on a simple lens."
          prop_modifyMap_basic_behavior
      , testProperty
          "`modifyMap` behaves properly on a complex lens."
          prop_modifyMap_advanced_behavior
      ]
  , testGroup
      "`id`"
      [ testProperty
          "`id` obeys the Get-Set law."
          prop_id_obeys_get_set
      , testProperty
          "`id` obeys the Set-Get law."
          prop_id_obeys_set_get
      , testProperty
          "`id` obeys the Set-Set law."
          prop_id_obeys_set_set
      , testProperty
          "`id` targets the source value."
          prop_id_targets_properly
      ]
  , testGroup
      "`(.)`"
      [ testProperty
          "`(.)` composes two lenses properly with respect to `get`."
          prop_compose_get_behavior
      , testProperty
          "`(.)` composes two lenses properly with respect to `set`."
          prop_compose_set_behavior
      , testProperty
          "`(.)` composes two lenses properly with respect to `modify`."
          prop_compose_modify_behavior
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
prop_modify_basic_behavior ::
     (Integer, String, IntSet) -> Fun IntSet IntSet -> Property
prop_modify_basic_behavior v@(n, s, nz) (Fn f) =
  modify testLensA f v === (n, s, f nz)

prop_modify_advanced_behavior :: Int -> IntSet -> Fun Bool Bool -> Property
prop_modify_advanced_behavior m nz (Fn f) =
  modify (testLensB m) f nz ===
  (if f (IS.member m nz)
     then IS.insert
     else IS.delete)
    m
    nz

prop_modifyMap_basic_behavior ::
     (Integer, String, IntSet) -> Fun IntSet [IntSet] -> Property
prop_modifyMap_basic_behavior v@(n, s, nz) (Fn f) =
  modifyMap testLensA f v === [(n, s, nz') | nz' <- f nz]

prop_modifyMap_advanced_behavior :: Int -> IntSet -> Fun Bool [Bool] -> Property
prop_modifyMap_advanced_behavior m nz (Fn f) =
  modifyMap (testLensB m) f nz ===
  [ (if p
       then IS.insert
       else IS.delete)
    m
    nz
  | p <- f (IS.member m nz)
  ]

prop_id_obeys_get_set :: Integer -> Property
prop_id_obeys_get_set s = set id s (get id s) === s

prop_id_obeys_set_get :: Integer -> Integer -> Property
prop_id_obeys_set_get s v = get id (set id s v) === v

prop_id_obeys_set_set :: Integer -> Integer -> Integer -> Property
prop_id_obeys_set_set s v v' = set id (set id s v) v' === set id s v'

prop_id_targets_properly :: Integer -> Fun Integer Integer -> Property
prop_id_targets_properly n (Fn f) = set id n (f (get id n)) === f n

prop_compose_get_behavior :: Int -> (Integer, String, IntSet) -> Property
prop_compose_get_behavior m v = get testLensBoA' v === get (testLensBoA m) v
  where
    testLensBoA' = testLensB m . testLensA

prop_compose_set_behavior ::
     Int -> (Integer, String, IntSet) -> Bool -> Property
prop_compose_set_behavior m v p =
  set testLensBoA' v p === set (testLensBoA m) v p
  where
    testLensBoA' = testLensB m . testLensA

prop_compose_modify_behavior ::
     Int -> (Integer, String, IntSet) -> Fun Bool Bool -> Property
prop_compose_modify_behavior m v (Fn f) =
  modify testLensBoA' f v === modify (testLensBoA m) f v
  where
    testLensBoA' = testLensB m . testLensA
