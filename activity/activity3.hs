import Prelude hiding (signum)

signum :: Int -> Int
signum n | (n > 0) = 1 | (n < 0) = -1 | otherwise = 0

if' :: Bool -> a -> a -> a
if' a b c | a == True = b | otherwise = c


data Tree = TInt Integer | TOp String Tree Tree deriving (Eq)

instance Show Tree where
   show (TInt i) = show i
   show (TOp s t1 t2) = "(" ++ s ++ " " ++ show t1 
                            ++ " " ++ show t2 ++ ")"

-- Your code here!
-- Feel free to write helper functions.
t1 = TOp "*" (TOp "+" (TInt 20) (TInt 1))
             (TOp "-" (TInt 10) (TInt 8))
t2 = TInt 10
--swap :: Tree -> Tree
--swap TOp String Tree Tree = 


calc :: Tree -> Integer
calc (TInt i) = i
calc (TOp f t1 t2) | f == "*" = t1' * t2'
                   | f == "+" = t1' + t2'
                   | f == "-" = t1' - t2'
                   | otherwise = error "Wrong operator"
 where t1' = calc t1
       t2' = calc t2

 