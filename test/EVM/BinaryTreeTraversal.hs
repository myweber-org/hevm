module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inorderTraversal :: BinaryTree a -> [a]
inorderTraversal tree = go tree []
  where
    go Leaf stack = stack
    go (Node left val right) stack = go left (val : go right stack)

preorderTraversal :: BinaryTree a -> [a]
preorderTraversal tree = go tree []
  where
    go Leaf stack = stack
    go (Node left val right) stack = val : go left (go right stack)

postorderTraversal :: BinaryTree a -> [a]
postorderTraversal tree = go tree []
  where
    go Leaf stack = stack
    go (Node left val right) stack = go left (go right (val : stack))

-- Example usage
exampleTree :: BinaryTree Int
exampleTree = Node (Node Leaf 2 Leaf) 1 (Node Leaf 3 Leaf)

main :: IO ()
main = do
  putStrLn $ "In-order: " ++ show (inorderTraversal exampleTree)
  putStrLn $ "Pre-order: " ++ show (preorderTraversal exampleTree)
  putStrLn $ "Post-order: " ++ show (postorderTraversal exampleTree)