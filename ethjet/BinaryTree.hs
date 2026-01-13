module BinaryTree where

data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

fromList :: Ord a => [a] -> BinaryTree a
fromList = foldr insert Empty

toSortedList :: BinaryTree a -> [a]
toSortedList = inorder

size :: BinaryTree a -> Int
size Empty = 0
size (Node left _ right) = 1 + size left + size right

contains :: Ord a => a -> BinaryTree a -> Bool
contains _ Empty = False
contains x (Node left val right)
    | x == val  = True
    | x < val   = contains x left
    | otherwise = contains x right