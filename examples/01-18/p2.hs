fact 0 = 1
fact n | n > 0 = n * fact (n-1)

remNeg [] = []
remNeg (x:xs) | x < 0     = result
              | otherwise = x : result
     where result = remNeg xs

myrev []     = []
myrev (x:xs) = myrev xs ++ [x]

myrev' xx = aux xx []
  where aux []     acc = acc
        aux (x:xs) acc = aux xs (x:acc)

decList (x:xs) = x - 1 : decList xs
decList []     = []

sumList []     a = a
sumList (x:xs) a = sumList xs $ a + x

incList []     a = reverse a
incList (x:xs) a = incList xs $ x+1 : a 

prodList xx = aux xx 0
  where aux []     a = 0
        aux (x:xs) a = aux xs (x * a)

maxList (x:xs) = aux xs x
  where aux []     a = a
        aux (x:xs) a = aux xs (max a x)

fact' n = aux n 1
   where aux 0 a  = a
         aux n a  = aux (n-1) (a * n)

all' p [] = True
all' p (x:xs) | p x       = all' p xs
              | otherwise = False

all2 p xx = aux xx
  where aux (x:xs) | p x       = aux xs
                   | otherwise = False

fib n = aux n 1 1
  where aux 0 f1 f2 = f1
        aux n f1 f2 = aux (n-1) f2 (f1 + f2)


