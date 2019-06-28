import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMain tests

expStr (IntExp i) = show i
expStr (VarExp s) = s
expStr (PlusExp e1 e2) = "+ " ++ expStr e1 ++ " " ++ expStr e2
expStr (LetExp s e1 e2) = "let " ++ s ++ " " ++ expStr e1 ++ " " ++ expStr e2

instance Arbitrary Exp where
    arbitrary = sized randSimpleExp
        where randSimpleExp n | n < 2 = oneof
                                        [ do e1 <- choose (0,1000)
                                             return $ IntExp e1
                                        , do e1 <- oneof (map return ["a","b","c","d","e","f","g"])
                                             return $ VarExp e1
                                        ]
                              | otherwise = oneof
                                            [ do m <- choose (0,n-1)
                                                 e1 <- randSimpleExp (n-m)
                                                 e2 <- randSimpleExp m
                                                 return $ PlusExp e1 e2
                                            , do m <- choose (1,n-1)
                                                 e1 <- randSimpleExp (n-m)
                                                 e2 <- randSimpleExp m
                                                 return $ LetExp "x" e1 e2
                                             ]
    shrink (IntExp _) = []
    shrink (VarExp _) = []
    shrink (PlusExp e1 e2) = [ e1, e2 ]
                             ++ (PlusExp <$> (shrink e1) <*> (shrink e2))
                             ++ (PlusExp <$> [e1] <*> (shrink e2))
                             ++ (PlusExp <$> (shrink e1) <*> [e2])
    shrink (LetExp v e1 e2) = [ e1, e2 ]
                             ++ (LetExp v <$> (shrink e1) <*> (shrink e2))
                             ++ (LetExp v <$> [e1] <*> (shrink e2))
                             ++ (LetExp v <$> (shrink e1) <*> [e2])

tests = [
        testGroup "=G= Test Group" [
                testProperty "=P= It Works (10 points)" prop1
           ]
      ]

prop1 e = let (e2,_) = parse (expStr e)
            in e2 == e
