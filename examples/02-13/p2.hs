-- News:
-- MP 2 Releases tonight
-- CPS activity -- problem 6

fact 0 = 1
fact n = n * fact (n-1)

fact' 0 k = k 1
fact' n k = fact' (n-1) (\v -> k $ n * v)

sumList [] k = k 0
sumList (x:xs) k = sumList xs (\v -> k $ x + v)

map' f [] k = k []
map' f (x:xs) k = map' f xs (\v -> k $ f x : v)

map'' f [] k = k []
map'' f (x:xs) k = map'' f xs (\vxs ->
                     f x (\vx -> k $ vx : vxs))

map''' f [] k = k []
map''' f (x:xs) k = f x (\vx ->
                     map''' f xs (\vxs -> k $ vx : vxs))

min' a b k = k $ if a < b then a else b
-- also, min' a b k = if a < b then k a else k b
min4 a b c d k = min' a b (\v1 ->
                    min' c d (\v2 ->
                    min' v1 v2 k))

-- fact 3 id
-- => fact 2 (\v1 -> id $ 3 * v1)
-- => fact 1 (\v2 -> (\v1 -> id $ 3 * v1) $ 2 * v2)
-- => fact 0 (\v3 -> (\v2 -> (\v1 -> id $ 3 * v1) $ 2 * v2) $ 1 * v3)
-- => (\v3 -> (\v2 -> (\v1 -> id $ 3 * v1) $ 2 * v2) $ 1 * v3) 1
-- => (\v2 -> (\v1 -> id $ 3 * v1) $ 2 * v2) $ 1 * 1
-- => (\v1 -> id $ 3 * v1) $ 2 * 1
-- => id $ 3 * 2
-- => 6


ifk c t e k =
  c (\v -> (case v of
             True -> t
             False -> e) k)
 
prodList xx k = aux xx k
  where aux []     k' = k' 1
        aux (0:xs) k' = k 0
        aux (x:xs) k' = aux xs (\v -> k' $ x * v)
  
