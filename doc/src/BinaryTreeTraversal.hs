module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

morrisInOrder :: BinaryTree a -> [a]
morrisInOrder Empty = []
morrisInOrder root = go root []
  where
    go Empty acc = acc
    go (Node val left right) acc =
      let current = left
      in case findPredecessor current val of
           Nothing -> val : go right acc
           Just predNode ->
             if rightChild predNode == Nothing
               then let predNode' = setRightChild predNode (Node val left right)
                    in go left (val : go right acc)
               else setRightChild predNode Empty
                 >> go right acc

findPredecessor :: BinaryTree a -> a -> Maybe (BinaryTree a)
findPredecessor Empty _ = Nothing
findPredecessor (Node v l r) target
  | v == target = Just (Node v l r)
  | otherwise = case findPredecessor l target of
                  Just found -> Just found
                  Nothing -> findPredecessor r target

rightChild :: BinaryTree a -> Maybe (BinaryTree a)
rightChild Empty = Nothing
rightChild (Node _ _ r) = Just r

setRightChild :: BinaryTree a -> BinaryTree a -> BinaryTree a
setRightChild Empty _ = Empty
setRightChild (Node v l _) newRight = Node v l newRight