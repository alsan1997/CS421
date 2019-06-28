mytake :: Int -> [a] -> [a]
mytake i [] = []
mytake i (x:xs) | i <= 0 = []
                | otherwise = x : mytake (i-1) xs

mydrop :: Int -> [a] -> [a]
mydrop i [] = []
mydrop i (x:xs) | i <= 0 = (x:xs)
                | otherwise = mydrop (i-1) xs   

rev :: [a] -> [a]
rev [] = []
rev xx@(x:xs) = aux xx []
    where aux [] f = f
          aux (x:xs) f = aux xs (x:f)

app :: [a] -> [a] -> [a]
app xx [] = xx
app [] xx = xx
app (x:xs) (y:ys) = x : app (xs) (y:ys)

inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs

sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs) = x + sumlist(xs)

myzip :: [a] -> [b] -> [(a,b)]
myzip [] b = []
myzip a [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

addpairs :: (Num a) => [a] -> [a] -> [a] 
addpairs [] b = []
addpairs a [] = []
addpairs xx yy = aux (myzip xx yy) 
    where aux [] = [] 
          aux ((x,y) : ps) = (x+y) : aux ps

ones :: [Integer] 
ones = 1 : ones

nats :: [Integer]
nats = [0..]

fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

add :: Ord a => a -> [a] -> [a]
add i [] = [i]
add i xx@(x:xs) | i == x = (x:xs)
                | i < x = (i:x:xs)
                | otherwise = x : add i xs 

union :: Ord a => [a] -> [a] -> [a] 
union [] [] = []
union a [] = a
union [] b = b
union (x:xs) (y:ys) | x < y = x : union xs (y:ys)
                    | x == y = x : union xs ys
                    | otherwise = y : union (x:xs) ys

intersect :: Ord a => [a] -> [a] -> [a]
intersect a [] = []
intersect [] b = []
intersect (x:xs) (y:ys) | x < y = intersect xs (y:ys)
                        | x == y = x : intersect xs ys
                        | otherwise = intersect (x:xs) ys

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union a (map(\a -> add x a) a)
    where a = powerset xs

list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : (cons2list xs)

list2cons :: [a] -> List a
list2cons = foldr (\a ls->Cons a ls ) Nil
 