
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- Example usage with a sample tree
sampleTree :: Tree Int
sampleTree = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 Empty Empty)

main :: IO ()
main = print $ treeHeight sampleTree