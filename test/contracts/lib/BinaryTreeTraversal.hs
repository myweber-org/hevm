module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

inorderCPS :: BinaryTree a -> ([a] -> r) -> r
inorderCPS Leaf cont = cont []
inorderCPS (Node left value right) cont =
    inorderCPS left $ \leftResult ->
    inorderCPS right $ \rightResult ->
    cont (leftResult ++ [value] ++ rightResult)

inorder :: BinaryTree a -> [a]
inorder tree = inorderCPS tree id

preorderCPS :: BinaryTree a -> ([a] -> r) -> r
preorderCPS Leaf cont = cont []
preorderCPS (Node left value right) cont =
    cont ([value] ++) $ \prefixFunc ->
    preorderCPS left $ \leftResult ->
    preorderCPS right $ \rightResult ->
    prefixFunc (leftResult ++ rightResult)

preorder :: BinaryTree a -> [a]
preorder tree = preorderCPS tree id

postorderCPS :: BinaryTree a -> ([a] -> r) -> r
postorderCPS Leaf cont = cont []
postorderCPS (Node left value right) cont =
    postorderCPS left $ \leftResult ->
    postorderCPS right $ \rightResult ->
    cont (leftResult ++ rightResult ++ [value])

postorder :: BinaryTree a -> [a]
postorder tree = postorderCPS tree id

sampleTree :: BinaryTree Int
sampleTree = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 Leaf)