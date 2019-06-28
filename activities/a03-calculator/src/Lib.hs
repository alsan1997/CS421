module Lib
    (Tree(TOp,TInt)
    ,swap
    ,calc) where

data Tree =
    TInt Integer
  | TOp String Tree Tree
  deriving (Eq)

instance Show Tree where
   show (TInt i) = show i
   show (TOp s t1 t2) = "(" ++ s ++ " " ++ show t1 
                            ++ " " ++ show t2 ++ ")"

-- Your code here!
-- Feel free to write helper functions.

swap :: Tree -> Tree
swap (TInt i) = (TInt i)
swap (TOp f l r) = TOp f r' l'
  where l' = swap l
        r' = swap r

calc :: Tree -> Integer
calc (TInt i) = i
calc (TOp f t1 t2) | f == "*" = t1' * t2'
                   | f == "+" = t1' + t2'
                   | f == "-" = t1' - t2'
                   | otherwise = error "Wrong operator"
 where t1' = calc t1
       t2' = calc t2
