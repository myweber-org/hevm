module BinaryTree where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq)

empty :: BinaryTree a
empty = Leaf

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

fromList :: Ord a => [a] -> BinaryTree a
fromList = foldr insert empty

toList :: BinaryTree a -> [a]
toList = inorder

size :: BinaryTree a -> Int
size Leaf = 0
size (Node left _ right) = 1 + size left + size right

contains :: Ord a => a -> BinaryTree a -> Bool
contains _ Leaf = False
contains x (Node left val right)
    | x == val  = True
    | x < val   = contains x left
    | otherwise = contains x right