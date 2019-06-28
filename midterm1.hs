decList [] = []
decList (x:xs) = (x - 1) : decList xs


decListTail xx = aux xx [] 
    where aux [] f = reverse(f)
          aux (x:xs) f = aux xs ((x-1):f)

data List a = Cons a (List a)
            | Nil
    deriving Show





data Foo a = Foo a
           | Bar
  deriving Show
instance Functor Foo where
   fmap f (Foo a) = Foo (f a)
   fmap f Bar = Bar

instance Applicative Foo where
   pure x = Foo x

   (Foo f) <*> (Foo x) = Foo (f x)
   _       <*> _       = Bar

instance Monad Foo where
     return a = Foo a
     (>>=) (Foo a) f = f a
     (>>=) _ _ = Bar

cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

countCons :: List a -> Int
countCons Nil = 0
countCons (Cons x xs) = 1 + countCons xs
 
mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (x:xs) | f x = mydropWhile f xs
                     | otherwise = (x:xs)

sumList xx = aux xx 0
    where aux [] a = a
          aux (x:xs) a = aux xs (a + x)

-- sumSqr :: [Integer] -> Int
-- sumSqr (x:xs) = foldr(\x y -> x*x + y) 0 (x:xs)

aboveFive :: [Integer] -> [Integer]
aboveFive (x:xs) = filter (\v-> v>5 && v<9) (x:xs)

maxk :: Integer -> Integer -> (Integer -> t) -> t
maxk a b k | (a > b) == True = k a
           | otherwise = k b

max3k :: Integer -> Integer -> Integer -> (Integer -> t) -> t
max3k a b c k = maxk b c (\v -> maxk a v (\v2 -> k(9 + v2))) 

 
flipzip xs ys = flip zip xs ys