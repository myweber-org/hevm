
module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node value left right) = inOrder left ++ [value] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node value left right) = [value] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node value left right) = postOrder left ++ postOrder right ++ [value]

sampleTree :: BinaryTree Int
sampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)