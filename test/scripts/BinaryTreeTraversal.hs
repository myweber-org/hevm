module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

morrisInOrder :: BinaryTree a -> [a]
morrisInOrder Empty = []
morrisInOrder root = go root []
  where
    go Empty acc = acc
    go (Node val left right) acc =
      let current = left
          findPredecessor node parent
            | node == Empty = parent
            | otherwise = findPredecessor (rightSub node) parent
            where rightSub (Node _ _ r) = r
          process node result =
            case node of
              Empty -> result
              Node v l r ->
                let pred = findPredecessor l node
                 in if rightSub pred == Empty
                      then
                        let newPred = Node v (rightSub pred) r
                         in process l (v : process (rightSub newPred) result)
                      else process l (v : result)
       in process (Node val left right) acc