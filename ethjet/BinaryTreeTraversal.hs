module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left value right) = [value] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left value right) = postorder left ++ postorder right ++ [value]

exampleTree :: BinaryTree Int
exampleTree = Node (Node Leaf 2 Leaf) 1 (Node (Node Leaf 4 Leaf) 3 Leaf)module BinaryTreeTraversal where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

inOrderCPS :: BinaryTree a -> ([a] -> r) -> r
inOrderCPS Leaf k = k []
inOrderCPS (Node left val right) k = 
    inOrderCPS left $ \leftList ->
        inOrderCPS right $ \rightList ->
            k (leftList ++ [val] ++ rightList)

inOrder :: BinaryTree a -> [a]
inOrder tree = inOrderCPS tree id

preOrderCPS :: BinaryTree a -> ([a] -> r) -> r
preOrderCPS Leaf k = k []
preOrderCPS (Node left val right) k = 
    k ([val] ++) $ \prefix ->
        preOrderCPS left $ \leftList ->
            preOrderCPS right $ \rightList ->
                prefix (leftList ++ rightList)

preOrder :: BinaryTree a -> [a]
preOrder tree = preOrderCPS tree id

postOrderCPS :: BinaryTree a -> ([a] -> r) -> r
postOrderCPS Leaf k = k []
postOrderCPS (Node left val right) k = 
    postOrderCPS left $ \leftList ->
        postOrderCPS right $ \rightList ->
            k (leftList ++ rightList ++ [val])

postOrder :: BinaryTree a -> [a]
postOrder tree = postOrderCPS tree id