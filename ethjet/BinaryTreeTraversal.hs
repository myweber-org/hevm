
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node val left right) = inOrder left ++ [val] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node val left right) = [val] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node val left right) = postOrder left ++ postOrder right ++ [val]

sampleTree :: BinaryTree Int
sampleTree = Node 1
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3
                    Empty
                    (Node 6 Empty Empty))data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right