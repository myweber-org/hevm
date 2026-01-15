module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node value left right) = [value] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node value left right) = postorder left ++ postorder right ++ [value]