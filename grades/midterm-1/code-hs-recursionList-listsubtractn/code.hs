listsubtractn :: [Int] -> Int -> [Int]
listsubtractn [] i = []
listsubtractn (x:xs) i = (x - i ) : listsubtractn xs i