
module BinarySearchTree where

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
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a)
    deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left y right)
    | x < y     = Node (insert x left) y right
    | x > y     = Node left y (insert x right)
    | otherwise = Node left y right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

toSortedList :: BST a -> [a]
toSortedList = inorder

main :: IO ()
main = do
    let tree = fromList [5, 3, 7, 2, 4, 6, 8]
    putStrLn "In-order traversal:"
    print $ toSortedList tree