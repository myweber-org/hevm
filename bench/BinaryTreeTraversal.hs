
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node x left right) = [x] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node val left right) = inOrder left ++ [val] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node val left right) = [val] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node val left right) = postOrder left ++ postOrder right ++ [val]

exampleTree :: BinaryTree Int
exampleTree = Node 1
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3
                    (Node 6 Empty Empty)
                    Empty)