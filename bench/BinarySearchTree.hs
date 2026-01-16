
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left y right)
    | x < y     = Node (insert x left) y right
    | x > y     = Node left y (insert x right)
    | otherwise = Node left y right

inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

toSortedList :: BST a -> [a]
toSortedList = inOrder
module BinarySearchTree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty