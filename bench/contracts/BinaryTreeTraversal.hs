module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left value right) = inOrder left ++ [value] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left value right) = [value] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left value right) = postOrder left ++ postOrder right ++ [value]

sampleTree :: BinaryTree Int
sampleTree = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 Leaf)