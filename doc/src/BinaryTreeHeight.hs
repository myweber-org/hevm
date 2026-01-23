module BinaryTreeHeight where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)module BinaryTreeHeight where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)