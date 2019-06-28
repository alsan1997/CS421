import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.List (permutations)
import qualified Data.HashSet as H

import Lib

main :: IO ()
main = defaultMain tests

tests = [ testGroup "=G= 'move' function"
            [ testProperty "=P= Must modify two stacks in one move"     prop_move_valid
            , testProperty "=P= Previous configuration is recorded"     prop_move_prev
            , testProperty "=P= All stacks are still sorted"            prop_move_sorted_stack
            , testProperty "=P= Must move to a different configuration" prop_move_out
            , testProperty "=P= Can go back by moving twice"            prop_move_back
            ]
        , testGroup "=G= 'moveSet' function"
            [ testProperty "=P= Results are reachable in one move"      prop_moveSet
            ]
        , testGroup "=G= 'oneMoveReachSet' function"
            [ testProperty "=P= Contain the given configurations"       prop_oneMoveReachSet
            ]
        , testGroup "=G= 'allReachSet' function"
            [ testProperty "=P= Contain all valid configuraions"        prop_allReachSet_getAllValid
            , testProperty "=P= Exclude any invalid configuration"      prop_allReachSet_noInvalid
            ]
        ]

newtype ArbValidHanoi = ArbValidHanoi Hanoi
instance Show ArbValidHanoi where
    show (ArbValidHanoi (Hanoi c _)) = "Given configuration: " ++ show c
instance Arbitrary ArbValidHanoi where
    arbitrary = do
        conf <- randValidConfig [3, 2, 1] ([], [], [])
        return $ ArbValidHanoi $ Hanoi conf []
        where randValidConfig [] conf = return conf
              randValidConfig (x:xs) (a, b, c) = do
                    i <- elements ['a', 'b', 'c']
                    case i of
                        'a' -> randValidConfig xs (x:a, b, c)
                        'b' -> randValidConfig xs (a, x:b, c)
                        'c' -> randValidConfig xs (a, b, x:c)

newtype ArbValidHanoiSet = ArbValidHanoiSet (H.HashSet Hanoi)
instance Show ArbValidHanoiSet where
    show (ArbValidHanoiSet cset) = "Given configuration set: " ++ show cset
instance Arbitrary ArbValidHanoiSet where
    arbitrary = do
             clist <- listOf (arbitrary :: Gen ArbValidHanoi)
             return $ ArbValidHanoiSet $ H.fromList $ map (\(ArbValidHanoi c) -> c) clist

newtype ArbInvalidHanoi = ArbInvalidHanoi Hanoi
instance Show ArbInvalidHanoi where
    show (ArbInvalidHanoi (Hanoi c _)) = "Given invalid configuration: " ++ show c
instance Arbitrary ArbInvalidHanoi where
    arbitrary = do
        r <- elements (permutations ([1..3]++[0,0]))
        let conf = split3 r
         in if not (allSorted conf) then
                return $ ArbInvalidHanoi $ Hanoi conf []
            else
                arbitrary :: Gen ArbInvalidHanoi
        where split3 [] = ([], [], [])
              split3 (0:xs) = let (a, b,[]) = split3 xs in ([],  a, b)
              split3 (x:xs) = let (a, b, c) = split3 xs in (x:a, b, c)

allSorted :: Config -> Bool
allSorted (a, b, c) = isSorted a && isSorted b && isSorted c where
    isSorted []       = True
    isSorted [x]      = True
    isSorted (x:y:xs) = x <= y && isSorted (y:xs)

validMove :: Config -> Config -> Bool
validMove (a, b, c) (a', b', c') -- TODO Stronger specification for valid moves
    =  (a == a' && b /= b' && c /= c')
    || (a /= a' && b == b' && c /= c')
    || (a /= a' && b /= b' && c == c')

validHanoiMove :: Hanoi -> Hanoi -> Bool
validHanoiMove (Hanoi conf _) (Hanoi conf' _)
    = validMove conf conf'

prop_move_valid :: ArbValidHanoi -> Property
prop_move_valid (ArbValidHanoi hanoi@(Hanoi conf _))
    = conjoin $ map aux (H.toList $ move hanoi) where
        aux g@(Hanoi conf' _) = counterexample ("Not a valid move to: " ++ show g) (validMove conf conf')

prop_move_prev :: ArbValidHanoi -> Property
prop_move_prev (ArbValidHanoi hanoi@(Hanoi conf p))
    = conjoin $ map aux (H.toList $ move hanoi) where
        aux g@(Hanoi _ p') = counterexample ("Wrong path after `move`: " ++ show g) (p' == conf:p)

prop_move_sorted_stack :: ArbValidHanoi -> Property
prop_move_sorted_stack (ArbValidHanoi hanoi)
    = conjoin $ map aux (H.toList $ move hanoi) where
        aux g@(Hanoi conf _) = counterexample ("Stacks are not sorted: " ++ show g) (allSorted conf)

prop_move_out :: ArbValidHanoi -> Property
prop_move_out (ArbValidHanoi hanoi)
    = let nextSet = move hanoi in
      counterexample ("Given configuration is in the result: " ++ show nextSet) $ not $ H.member hanoi nextSet

prop_move_back :: ArbValidHanoi -> Property
prop_move_back (ArbValidHanoi hanoi)
    = conjoin $ map aux (H.toList $ move hanoi) where
        aux g = counterexample ("Cannot move back from configuration: " ++ show g) (H.member hanoi $ move g)

prop_moveSet :: ArbValidHanoiSet -> Property
prop_moveSet (ArbValidHanoiSet cset)
    = conjoin $ map aux (H.toList $ moveSet cset) where
        aux g = counterexample ("Following configuration should not be in moveSet: " ++ show g) (any (validHanoiMove g) (H.toList cset))

prop_oneMoveReachSet :: ArbValidHanoiSet -> Property
prop_oneMoveReachSet (ArbValidHanoiSet cset)
    = let res = H.difference cset (oneMoveReachSet cset)
       in counterexample ("Given configuration(s) not in oneMoveReachSet: " ++ show res) (H.null res)

prop_allReachSet_getAllValid :: ArbValidHanoi -> Property
prop_allReachSet_getAllValid (ArbValidHanoi hanoi)
    = counterexample "Given valid configuration is unreachable." (H.member hanoi allReachSet)

prop_allReachSet_noInvalid :: ArbInvalidHanoi -> Property
prop_allReachSet_noInvalid (ArbInvalidHanoi hanoi)
    = counterexample "Given invalid configuration is reachable." (not $ H.member hanoi allReachSet)
