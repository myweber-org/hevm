
module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left value right) = [value] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left value right) = postorder left ++ postorder right ++ [value]data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inOrderTraversal :: BinaryTree a -> [a]
inOrderTraversal tree = go tree []
  where
    go Leaf stack = stack
    go (Node left val right) stack = go left (val : go right stack)