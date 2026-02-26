module BinaryTreeDepth where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

treeDepth :: BinaryTree a -> Int
treeDepth Leaf = 0
treeDepth (Node left _ right) = 1 + max (treeDepth left) (treeDepth right)data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

sampleTree :: BinaryTree Int
sampleTree = Node 1 
                (Node 2 
                    (Node 4 Empty Empty) 
                    (Node 5 Empty Empty))
                (Node 3 
                    Empty 
                    (Node 6 Empty Empty))

main :: IO ()
main = do
    putStrLn $ "Depth of sample tree: " ++ show (treeDepth sampleTree)
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

sampleTree :: BinaryTree Int
sampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty Empty))data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

treeDepth :: BinaryTree a -> Int
treeDepth Leaf = 0
treeDepth (Node left _ right) = 1 + max (treeDepth left) (treeDepth right)data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)