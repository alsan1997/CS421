maxList :: [Int] -> Int
maxList (x:xs) = aux (x:xs) 0
    where aux [] a = a
          aux (x:xs) a = aux xs (max a x)