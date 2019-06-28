-- Yay, the stoopid projector works!
-- Echo360 should be working either today or tomorrow.
-- New activity to be released tonight after second section.
-- Exam Research Opportunity...

data MyList a = Cons a (MyList a)
              | EndOfList
     deriving (Show)

nats 0 = EndOfList
nats n = Cons n (nats (n-1))

-- mysum :: MyList Integer -> Integer
mysum EndOfList = 0
-- mysum (Cons a (Cons b c)) = a + mysum c
mysum (Cons a b) = a + mysum b

-- problem 1
-- can't do it!

tupLen (x,y) = 2
-- tupLen (x,y,z) = 3

-- problem 2
assoc k v [] = [(k,v)]
assoc k v xx@((k2,v2):xs)
    | k < k2    = (k,v):xx
    | otherwise = (k2,v2) : assoc k v xs

-- problem 3
get 0 0 ((a,b),(c,d)) = a
get 1 0 ((a,b),(c,d)) = c
get 0 1 ((a,b),(c,d)) = b
get 1 1 ((a,b),(c,d)) = d

-- problem 4
data Direction = GoLeft | GoRight

get2 GoLeft GoLeft ((a,b),(c,d)) = a
get2 GoRight GoLeft ((a,b),(c,d)) = c
get2 GoLeft GoRight ((a,b),(c,d)) = b
get2 GoRight GoRight ((a,b),(c,d)) = d

-- Problem 5

data MyMaybe a = MyJust a
               | Nada
         deriving (Show)

maybePlus (MyJust x) (MyJust y) = MyJust $ x + y
maybePlus _ _ = Nada

lift f (MyJust x) (MyJust y) = MyJust $ f x y
lift f _ _ = Nada

data Tree a = Node a (Tree a) (Tree a)
            | Empty
     deriving (Show)

del victim Empty = Empty
del victim n@(Node x Empty Empty)
    | x == victim  = Empty
    | otherwise = n
del victim n@(Node x Empty c)
    | x <= victim  = c
    | otherwise = Node x Empty (del victim c)
del victim n@(Node x c Empty)
    | x > victim  = c
    | otherwise = Node x (del victim c) Empty

