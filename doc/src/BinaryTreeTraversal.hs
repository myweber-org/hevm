
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

inorder :: BinaryTree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Empty = []
preorder (Node x left right) = [x] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Empty = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]