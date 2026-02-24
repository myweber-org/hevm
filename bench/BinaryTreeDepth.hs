data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

treeDepth :: BinaryTree a -> Int
treeDepth Leaf = 0
treeDepth (Node left _ right) = 1 + max (treeDepth left) (treeDepth right)