module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left val right) = [val] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left val right) = postorder left ++ postorder right ++ [val]