data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

sampleTree :: Tree Int
sampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)