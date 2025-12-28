
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeDepth :: BinaryTree a -> Int
treeDepth EmptyTree = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)