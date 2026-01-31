module BinarySearchTree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

lookup :: Ord a => a -> Tree a -> Bool
lookup _ Empty = False
lookup x (Node y left right)
    | x < y     = BinarySearchTree.lookup x left
    | x > y     = BinarySearchTree.lookup x right
    | otherwise = True

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right