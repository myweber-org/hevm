
module BinarySearchTree where

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
    print tree
    putStrLn "In-order traversal:"
    print $ inOrder tree