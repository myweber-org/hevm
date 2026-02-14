
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node val left right) = [val] ++ preorder left ++ preorder right
module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node value left right) = [value] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node value left right) = postorder left ++ postorder right ++ [value]

levelOrder :: BinaryTree a -> [a]
levelOrder tree = levelOrderHelper [tree]
  where
    levelOrderHelper [] = []
    levelOrderHelper (Empty : xs) = levelOrderHelper xs
    levelOrderHelper (Node value left right : xs) = value : levelOrderHelper (xs ++ [left, right])