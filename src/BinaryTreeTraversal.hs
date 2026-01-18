module BinaryTreeTraversal where

data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

inOrderIterative :: BinaryTree a -> [a]
inOrderIterative tree = go [] tree []
  where
    go stack Empty result =
      case stack of
        [] -> reverse result
        (Node Empty val right):rest -> go rest right (val:result)
        (Node left val right):rest -> go (Node Empty val right:stack) left result
    go stack (Node left val right) result = go (Node left val right:stack) left resultmodule BinaryTreeTraversal where

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

exampleTree :: BinaryTree Int
exampleTree = Node 1
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3
                    (Node 6 Empty Empty)
                    Empty)data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left val right) = [val] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left val right) = postorder left ++ postorder right ++ [val]