dotproduct :: (Num a) => [a] -> [a] -> a
dotproduct [] [] = 0
dotproduct (x:xs) (y:ys) = x*y + (dotproduct xs ys)

data List a = Cons a (List a)
            | Nil
  deriving Show

cons2list :: List a -> [a]
cons2list (Nil) = []
cons2list (Cons x xs) = x : cons2list xs 

countCons :: List a -> Int
countCons = undefined

removeEvens :: [Int] -> [Int]
removeEvens [] = []
removeEvens (x:xs) | x `mod` 2 == 0 = removeEvens xs
                   | otherwise = x : removeEvens xs


mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f xx@(x:xs) | f x = mydropWhile f xs
                        | otherwise = xx

maxList :: [Integer] -> Integer
maxList xx@ (x:xs) = foldr(\x y-> if x > y then x else y) x xx




removeEvensHOF xx = takeWhile even xx