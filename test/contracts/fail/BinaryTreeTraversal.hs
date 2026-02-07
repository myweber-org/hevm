
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node val left right) = inorder left ++ [val] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node val left right) = postorder left ++ postorder right ++ [val]

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node val left right) = [val] ++ preorder left ++ preorder right