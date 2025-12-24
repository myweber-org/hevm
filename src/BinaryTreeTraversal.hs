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
    go stack (Node left val right) result = go (Node left val right:stack) left result