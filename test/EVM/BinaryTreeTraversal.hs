module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

inorderCPS :: BinaryTree a -> ([a] -> r) -> r
inorderCPS Leaf k = k []
inorderCPS (Node left val right) k =
    inorderCPS left $ \leftList ->
    inorderCPS right $ \rightList ->
    k (leftList ++ [val] ++ rightList)

inorder :: BinaryTree a -> [a]
inorder tree = inorderCPS tree id

preorderCPS :: BinaryTree a -> ([a] -> r) -> r
preorderCPS Leaf k = k []
preorderCPS (Node left val right) k =
    k ([val] ++) $ \prefix ->
    preorderCPS left $ \leftList ->
    preorderCPS right $ \rightList ->
    prefix (leftList ++ rightList)

preorder :: BinaryTree a -> [a]
preorder tree = preorderCPS tree id

postorderCPS :: BinaryTree a -> ([a] -> r) -> r
postorderCPS Leaf k = k []
postorderCPS (Node left val right) k =
    postorderCPS left $ \leftList ->
    postorderCPS right $ \rightList ->
    k (leftList ++ rightList ++ [val])

postorder :: BinaryTree a -> [a]
postorder tree = postorderCPS tree id

sampleTree :: BinaryTree Int
sampleTree = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 Leaf)