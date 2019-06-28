data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

add_bst :: Integer -> Tree Integer -> Tree Integer
add_bst i Empty = Node i Empty Empty
add_bst i (Node x left right)
 | i <= x = Node x (add_bst i left) right
 | otherwise = Node x left (add_bst i right)