import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Swap Function" [
                testProperty "Is its own inverse" (forAll arbitrary ownInverse)
           ,    testProperty "Preserves Subtrees" (forAll arbitrary preservesSubtrees)
           ]
           ,
        testGroup "Calc Function" [
                testProperty "Handles leaves" (forAll (arbitrarySizedTree 0) correctResult)
              , testProperty "Handles plus"  (forAll (sized $ rootedTree "+") correctResult)
              , testProperty "Handles times"  (forAll (sized $ rootedTree "-") correctResult)
              , testProperty "Handles minus"  (forAll (sized $ rootedTree "*") correctResult)
           ]
      ]

instance Arbitrary Tree where
  arbitrary = sized arbitrarySizedTree
  shrink (TInt i) = []
  shrink (TOp op t1 t2) = [TInt 1,t1,t2] ++ [TOp op x' y' |  (x',y') <- shrink (t1,t2)]

arbitrarySizedTree :: Int -> Gen Tree
arbitrarySizedTree 0 = do
  i <- arbitrary
  return $ TInt i

arbitrarySizedTree n = do
  theOp <- oneof [return "*",return "+",return "-"]
  subSize <- choose (0,n-1)
  t1 <- arbitrarySizedTree subSize
  t2 <- arbitrarySizedTree (max (n - subSize - 1) 0)
  return (TOp theOp t1 t2)

rootedTree :: String -> Int -> Gen Tree
rootedTree op n = do
  subSize <- choose (0,n-1)
  t1 <- arbitrarySizedTree subSize
  t2 <- arbitrarySizedTree (max (n - subSize - 1) 0)
  return (TOp op t1 t2)

ownInverse :: Tree -> Bool
ownInverse t = swap (swap t) == t

preservesSubtrees :: Tree -> Bool
preservesSubtrees t@(TOp op t1 t2) =
     let (TOp _ t2' t1') = swap t
      in swap t2' == t2 || swap t1' == t1
preservesSubtrees _ = True

ops = [("+",(+)), ("-",(-)), ("*",(*))]
 
correctResult t@(TInt i) = calc t == i
correctResult t@(TOp op t1 t2) =
   let c1 = calc t1
       c2 = calc t2
       Just f = lookup op ops
    in calc t == f c1 c2
