--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake i [] = []
mytake i xx@(x:xs) | i <= 0 = [] 
                   | otherwise = x : mytake (i-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop i [] = []
mydrop i xx@(x:xs) | i <= 0 = xx
                   | otherwise = mydrop (i - 1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []

rev xx = aux xx []
  where aux :: [a] -> [a] -> [a]
        aux [] re = re 
        aux (x:xs) re = aux xs (x:re)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] a = a
app a [] = a
app (x:xs) b = x : (app xs b)
 
--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: (Num a) => [a] -> a 
sumlist [] = 0
sumlist (x:xs) = x + sumlist(xs)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)] 
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip _ _ = []

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xx yy = aux (myzip xx yy)
    where aux :: (Num a) => [(a,a)] -> [a]
          aux [] = []
          aux ((x,y):ps) = (x+y):aux ps

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add i [] = [i]
add i xx@(x:xs) | i == x = (x:xs)
                | i < x = i:x:xs
                | otherwise = x : add i xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a] 
union xx [] = xx
union [] yy = yy

union (x:xs) (y:ys) | x == y = x : union xs ys
                    | x < y = x : union xs (y:ys)
                    | otherwise = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xx [] = []
intersect [] yy = []

intersect (x:xs) (y:ys) | x < y = intersect xs (y:ys)
                        | x == y = x : intersect xs ys
                        | otherwise = intersect (x:xs) ys

add_elem_to_each_list :: Ord a => [[a]] -> a -> [[a]]
add_elem_to_each_list [] x = []
add_elem_to_each_list (a:as) x = (add x a) : (add_elem_to_each_list as x)

aetel_map :: Ord a => [[a]] -> a -> [[a]]
aetel_map l x = map (\a -> add x a) l


--- ### powerset
-- OH
-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union a (aetel_map a x)
    where a = powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' xx = map(\x -> (x+1)) xx

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = foldr (+) 0

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

--- ### eval
-- OH
-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp i) = i
eval (PlusExp []) = 0
eval (MultExp []) = 1
eval (PlusExp (x:xs)) = (eval (x)) + (eval (PlusExp xs))
eval (MultExp (x:xs)) = (eval (x)) * (eval (MultExp xs))

--- ### list2cons'
--OH
-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr (\a ls -> Cons a ls)  Nil

--- ### BinTree
data BinTree a = Node a (BinTree a) (BinTree a) 
               | Leaf 
  deriving (Show)

--- ### sumTree
-- OH
-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x y z) = x + sumTree(y) + sumTree(z)

--- ### SimpVal

data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
        deriving (Show)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp i (IntVal a) (IntVal b) = IntVal (i a b)
liftIntOp i _ _ = ExnVal "not an IntVal!" 
