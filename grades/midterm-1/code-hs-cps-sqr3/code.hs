sqrk :: Integer -> (Integer -> t) -> t
sqrk a k = k(a*a)

sqrkmin :: Integer -> Integer -> (Integer -> t) -> t
sqrkmin a b k | ((sqrk a id) < (sqrk b id)) == True = k a
              | otherwise = k b        