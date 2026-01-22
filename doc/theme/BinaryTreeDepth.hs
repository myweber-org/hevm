data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

treeDepth :: BinaryTree a -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

sampleTree :: BinaryTree Int
sampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty (Node 6 Empty Empty))

main :: IO ()
main = do
    let depth = treeDepth sampleTree
    putStrLn $ "Maximum depth of the sample tree is: " ++ show depth