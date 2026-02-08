
module BinarySearchTree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

toSortedList :: Tree a -> [a]
toSortedList = inordermodule BinarySearchTree where

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

main :: IO ()
main = do
    let tree = fromList [5, 3, 8, 1, 4, 7, 9]
    print $ inorder treemodule BinarySearchTree where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

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

main :: IO ()
main = do
    let tree = fromList [5, 3, 7, 1, 4, 6, 8]
    putStrLn "In-order traversal:"
    print $ inOrder tree