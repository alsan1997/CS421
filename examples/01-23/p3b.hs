-- Echo360 should be working either today or tomorrow.
-- New activity to be released tonight after second section.
-- Exam Research Opportunity...

-- Chicago peeps!  Online office hours on Tuesday
--   at 10:00 in Illini Room B!  (That's 10am....)

-- template<class a> Mylist<a>

data MyList a = Nil
              | Cons a (MyList a)
   deriving (Show)

nats 0 = Nil
nats n = Cons n (nats $ n-1)

mysum Nil = 0
mysum (Cons x y) = x + mysum y


-- problem 1
-- can't do it

tupLen (_,_) = 2

-- problem 2

assoc k v [] = [(k,v)]
assoc k v n@((k2,v2):xs)
    | k < k2    = (k,v):n
    | otherwise = (k2,v2):assoc k v xs

-- problem 3

get 0 0 ((a,b),(c,d)) = a
get 1 0 ((a,b),(c,d)) = c
get 0 1 ((a,b),(c,d)) = b
get 1 1 ((a,b),(c,d)) = d

data Direction = GoLeft | GoRight

get2 GoLeft GoLeft ((a,b),(c,d)) = a
get2 GoRight GoLeft ((a,b),(c,d)) = c
get2 GoLeft GoRight ((a,b),(c,d)) = b
get2 GoRight GoRight ((a,b),(c,d)) = d

-- problem 4


-- data Maybe a = Nothing
--             | Just a


maybePlus (Just a) (Just b) = Just $ a + b
maybePlus _        _        = Nothing

m1 = Just 39
m2 = Nothing
m3 = Just 3

lift f (Just a) (Just b) = Just $ f a b
lift _ _        _        = Nothing
