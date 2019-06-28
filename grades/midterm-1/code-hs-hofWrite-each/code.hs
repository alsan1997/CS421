myeach :: (a -> Bool) -> [a] -> Bool
myeach f [] = True
myeach f (x:xs) | f x == True = myeach f xs
                | otherwise = False