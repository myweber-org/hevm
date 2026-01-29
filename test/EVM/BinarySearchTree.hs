
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

search :: Ord a => a -> BST a -> Bool
search _ Empty = False
search x (Node left val right)
    | x < val   = search x left
    | x > val   = search x right
    | otherwise = True

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right