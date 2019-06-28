-- ACTIVITY IS TWO PAGES TODAY


Just f <*> Just x -->  Just (f x)

 (Just x) >>= f -->  f x

Problem 1 typo::

  join (Just Nothing)    -> Nothing
  join (Just (Just 340)) -> Just 340
  join Nothing           -> Nothing

join [] = []
join ((x:xs):ys) = x : join (xs:ys)

join Nothing = Nothing
join (Just x) = x

(Just x) >>= f = join (Just (f x))
(Just x) >>= f = f x

  join (Just (f x)) ==  f x


