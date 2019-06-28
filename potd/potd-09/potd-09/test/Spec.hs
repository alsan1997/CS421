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
      "`fstLens`"
      [ testProperty
          "`fstLens` obeys the Get-Set law."
          prop_fstLens_obeys_get_set
      , testProperty
          "`fstLens` obeys the Set-Get law."
          prop_fstLens_obeys_set_get
      , testProperty
          "`fstLens` obeys the Set-Set law."
          prop_fstLens_obeys_set_set
      , testProperty
          "`fstLens` targets the first element of pairs."
          prop_fstLens_targets_properly
      ]
  , testGroup
      "`sndLens`"
      [ testProperty
          "`sndLens` obeys the Get-Set law."
          prop_sndLens_obeys_get_set
      , testProperty
          "`sndLens` obeys the Set-Get law."
          prop_sndLens_obeys_set_get
      , testProperty
          "`sndLens` obeys the Set-Set law."
          prop_sndLens_obeys_set_set
      , testProperty
          "`sndLens` targets the second element of pairs."
          prop_sndLens_targets_properly
      ]
  , testGroup
      "`entryLens`"
      [ testProperty
          "`entryLens` obeys the Get-Set law."
          prop_entryLens_obeys_get_set
      , testProperty
          "`entryLens` obeys the Set-Get law."
          prop_entryLens_obeys_set_get
      , testProperty
          "`entryLens` obeys the Set-Set law."
          prop_entryLens_obeys_set_set
      , testProperty
          "`entryLens` targets the specified member of maps."
          prop_entryLens_targets_properly
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
prop_fstLens_obeys_get_set :: (Integer, String) -> Property
prop_fstLens_obeys_get_set s = set fstLens s (get fstLens s) === s

prop_fstLens_obeys_set_get :: (Integer, String) -> Integer -> Property
prop_fstLens_obeys_set_get s v = get fstLens (set fstLens s v) === v

prop_fstLens_obeys_set_set ::
     (Integer, String) -> Integer -> Integer -> Property
prop_fstLens_obeys_set_set s v v' =
  set fstLens (set fstLens s v) v' === set fstLens s v'

prop_fstLens_targets_properly ::
     (Integer, String) -> Fun Integer Integer -> Property
prop_fstLens_targets_properly v@(n, s) (Fn f) =
  set fstLens v (f (get fstLens v)) === (f n, s)

prop_sndLens_obeys_get_set :: (Integer, String) -> Property
prop_sndLens_obeys_get_set s = set sndLens s (get sndLens s) === s

prop_sndLens_obeys_set_get :: (Integer, String) -> String -> Property
prop_sndLens_obeys_set_get s v = get sndLens (set sndLens s v) === v

prop_sndLens_obeys_set_set :: (Integer, String) -> String -> String -> Property
prop_sndLens_obeys_set_set s v v' =
  set sndLens (set sndLens s v) v' === set sndLens s v'

prop_sndLens_targets_properly ::
     (Integer, String) -> Fun String String -> Property
prop_sndLens_targets_properly v@(n, s) (Fn f) =
  set sndLens v (f (get sndLens v)) === (n, f s)

prop_entryLens_obeys_get_set :: Integer -> Map Integer String -> Property
prop_entryLens_obeys_get_set i s = set l s (get l s) === s
  where
    l = entryLens i

prop_entryLens_obeys_set_get ::
     Integer -> Map Integer String -> Maybe String -> Property
prop_entryLens_obeys_set_get i s v = get l (set l s v) === v
  where
    l = entryLens i

prop_entryLens_obeys_set_set ::
     Integer -> Map Integer String -> Maybe String -> Maybe String -> Property
prop_entryLens_obeys_set_set i s v v' = set l (set l s v) v' === set l s v'
  where
    l = entryLens i

prop_entryLens_targets_properly ::
     Integer
  -> Map Integer String
  -> Fun (Maybe String) (Maybe String)
  -> Property
prop_entryLens_targets_properly i vm (Fn f) =
  set l vm (f (get l vm)) === M.alter f i vm
  where
    l = entryLens i
