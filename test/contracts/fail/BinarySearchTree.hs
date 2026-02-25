
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val   = Node (insert x left) val right
    | x > val   = Node left val (insert x right)
    | otherwise = Node left val right

inOrder :: BST a -> [a]
inOrder Empty = []
inOrder (Node left val right) = inOrder left ++ [val] ++ inOrder right

buildTree :: Ord a => [a] -> BST a
buildTree = foldr insert Empty

main :: IO ()
main = do
    let tree = buildTree [5, 3, 7, 1, 4, 6, 8]
    print tree
    putStrLn "In-order traversal:"
    print $ inOrder tree