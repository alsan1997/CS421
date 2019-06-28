-- POTD 3 released tomorrow at noon
-- POTD 4 released Monday

-- kill the hydra!!


chop [] = []
chop (0:xs) = 0 : chop xs
chop [n] = [n-1]
chop (x:y:xs) = x-1 : y + l : xs
   where l = length xs + 1
--  [0,0,0,0,x  ,y  ,-,-,-,-,]
--  [0,0,0,0,x-1,y+l,-,-,-,-,]

negList xx = map (\x -> -x) xx

neg x = -x
negList' xx = map neg xx

negList3 = map (\x -> -x)

--   (\x -> x + 1) 10  ==>  10 + 1
--   (\x -> \y -> x + y) 10 20  ==>  (\y -> 10 + y) 20 ==> 10 + 20
--   (\x y -> x + y) 10 20 

sumSqr = foldr (\a b -> a*a+b) 0


zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _      _      = []

fibs = 1:1:zipWith' (+) fibs (tail fibs)

fix f x =
  if x == result
    then x
    else fix f result
  where result = f x

fix' f eps x = aux x
  where aux x | (abs $ x - result) =  x
              | otherwise = aux result
        result = f x
