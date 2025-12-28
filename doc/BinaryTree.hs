
module BinaryTree where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]