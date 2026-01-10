
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

treeDepth :: BinaryTree a -> Int
treeDepth EmptyTree = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

sampleTree :: BinaryTree Int
sampleTree = Node 1
                (Node 2
                    (Node 4 EmptyTree EmptyTree)
                    (Node 5 EmptyTree EmptyTree))
                (Node 3
                    EmptyTree
                    (Node 6 EmptyTree EmptyTree))