
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
                    Empty)module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal tree = go tree []
  where
    go Empty stack = stack
    go (Node val left right) stack = go left (val : go right stack)

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal tree = go tree []
  where
    go Empty stack = stack
    go (Node val left right) stack = val : go left (go right stack)

postorderTraversal :: BinaryTree a -> [a]
postorderTraversal tree = go tree []
  where
    go Empty stack = stack
    go (Node val left right) stack = go left (go right (val : stack))

-- Example usage
exampleTree :: BinaryTree Int
exampleTree = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)

testTraversals :: IO ()
testTraversals = do
    putStrLn "In-order traversal:"
    print $ inorderTraversal exampleTree
    putStrLn "Pre-order traversal:"
    print $ preorderTraversal exampleTree
    putStrLn "Post-order traversal:"
    print $ postorderTraversal exampleTree