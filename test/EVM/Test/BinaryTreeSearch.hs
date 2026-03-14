module BinaryTreeSearch where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

search :: Ord a => a -> BinaryTree a -> Bool
search _ Empty = False
search x (Node y left right)
    | x == y    = True
    | x < y     = search x left
    | otherwise = search x right

fromList :: Ord a => [a] -> BinaryTree a
fromList = foldr insert Empty

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right