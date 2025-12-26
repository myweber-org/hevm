
module BinarySearchTree where

data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node Empty x Empty
insert x (Node left val right)
    | x < val  = Node (insert x left) val right
    | x > val  = Node left val (insert x right)
    | otherwise = Node left val right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node left val right) = inorder left ++ [val] ++ inorder right

fromList :: Ord a => [a] -> BST a
fromList = foldr insert Empty

main :: IO ()
main = do
    let tree = fromList [5, 3, 7, 1, 4, 6, 8]
    putStrLn "Inorder traversal:"
    print $ inorder tree