module BinarySearchTree where

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node left val right) = inOrder left ++ [val] ++ inOrder right

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

toSortedList :: Ord a => Tree a -> [a]
toSortedList = inOrder