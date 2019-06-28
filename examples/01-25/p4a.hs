
chop [] = []
chop (0:xx) = 0 : chop xx
chop [n] = [n-1]
chop (x:y:xx) = x-1 : y+(length xx)+1 : xx

foldr (\a b -> ...) z [1,2,3]

foldr (+) 0 [1,2,3]
(+) 1 (foldr (+) 0 [2,3])
(+) 1 ((+) 2 (foldr (+) 0 [3]))
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 []))
(+) 1 ((+) 2 ((+) 3 0)
(+) 1 ((+) 2 3)
(+) 1 5
6

neg x = - x
negList xx = map neg xx
negList xx = map (\x -> -x) xx
negList xx = map (* (-1)) xx

   (\x ->  x * x ) 10
   (\ x -> \y -> x * y ) 10 20
    ==>  (\y -> 10 * y) 20
==>  10 * 20
(\x y -> x * y)
sumSqr = foldr (\x y -> x*x + y) 0

zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith f _      _      = []

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

tail (x:xs) = xs

foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

fix f x =
   if x == result
     then result
else fix f result
   where result = f x

fix f eps x = aux x
   where aux x =
            let result = f x
             in if (abs $ result - x) < eps then ....

[0,0,0,0,x,  y  ,-,-,-,-]
[0,0,0,0,x-1,y+l,-,-,-,-]
