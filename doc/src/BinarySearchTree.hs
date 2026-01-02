
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

toSortedList :: BST a -> [a]
toSortedList = inorder

contains :: Ord a => a -> BST a -> Bool
contains _ Empty = False
contains x (Node left val right)
    | x == val  = True
    | x < val   = contains x left
    | otherwise = contains x rightmodule BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node left val right) = inOrder left ++ [val] ++ inOrder right

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

toSortedList :: BST a -> [a]
toSortedList = inOrder