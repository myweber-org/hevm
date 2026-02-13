
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node val left right) = [val] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

exampleTree :: BinaryTree Int
exampleTree = Node 1
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3
                    (Node 6 Empty Empty)
                    Empty)module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left value right) = [value] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left value right) = postorder left ++ postorder right ++ [value]