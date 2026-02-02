
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = inorder left ++ [value] ++ inorder right

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left value right) = [value] ++ preorder left ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left value right) = postorder left ++ postorder right ++ [value]
module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

inorderMorris :: BinaryTree a -> [a]
inorderMorris Empty = []
inorderMorris root = go root []
  where
    go Empty acc = acc
    go (Node val left right) acc =
      let current = left
          processNode node result =
            case node of
              Empty -> result
              Node v l r ->
                let predecessor = findPredecessor l v
                in case predecessor of
                     Nothing -> processNode l (v : processNode r result)
                     Just (predVal, predRight) ->
                       if predRight == node
                         then processNode l (v : processNode r result)
                         else processNode l (predVal : processNode r result)
      in processNode current (val : go right acc)

    findPredecessor :: BinaryTree a -> a -> Maybe (a, BinaryTree a)
    findPredecessor Empty _ = Nothing
    findPredecessor (Node v l r) target =
      if r == target
        then Just (v, r)
        else case findPredecessor l target of
               Nothing -> findPredecessor r target
               result -> result

createSampleTree :: BinaryTree Int
createSampleTree =
  Node 4
    (Node 2
      (Node 1 Empty Empty)
      (Node 3 Empty Empty))
    (Node 6
      (Node 5 Empty Empty)
      (Node 7 Empty Empty))