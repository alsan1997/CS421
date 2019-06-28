module Lib where

import Data.Hashable
import qualified Data.HashSet as H

type Stack  = [Int]
type Config = (Stack, Stack, Stack)
data Hanoi  = Hanoi Config [Config]
    deriving Show

instance Eq Hanoi where
    (==) (Hanoi c _) (Hanoi c' _) = c == c'

instance Hashable Hanoi where
    hashWithSalt s (Hanoi c _) = s `hashWithSalt` c

fix' :: Eq a => (a -> a) -> a -> a
fix' f x = if g == x
          then g
          else fix' f g
    where g = f x

initConf = ([1,2,3], [], [])
initSet = H.singleton (Hanoi initConf [])

findPath :: Config -> Maybe [Config]
findPath conf
    = case H.toList $ H.filter (Hanoi conf [] ==) allReachSet of
        [Hanoi _ path] -> Just $ reverse $ conf:path
        _   -> Nothing

-- Your code here!
move :: Hanoi -> H.HashSet Hanoi
move g@(Hanoi conf@(aStack, bStack, cStack) p) = undefined

moveSet :: H.HashSet Hanoi -> H.HashSet Hanoi
moveSet s = undefined

oneMoveReachSet :: H.HashSet Hanoi -> H.HashSet Hanoi
oneMoveReachSet s = undefined

allReachSet :: H.HashSet Hanoi
allReachSet = undefined
