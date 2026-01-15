module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inorderCPS :: BinaryTree a -> ([a] -> r) -> r
inorderCPS Leaf k = k []
inorderCPS (Node left val right) k =
    inorderCPS left (\leftResult ->
        inorderCPS right (\rightResult ->
            k (leftResult ++ [val] ++ rightResult)))

inorder :: BinaryTree a -> [a]
inorder tree = inorderCPS tree id

sampleTree :: BinaryTree Int
sampleTree = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 (Node Leaf 5 Leaf))