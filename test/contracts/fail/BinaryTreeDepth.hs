data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)