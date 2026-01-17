
module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

inorderMorris :: BinaryTree a -> [a]
inorderMorris Empty = []
inorderMorris root = go root []
  where
    go Empty acc = acc
    go (Node val left right) acc =
      let current = left
          goLeft node prevAcc =
            case node of
              Empty -> (val : go right prevAcc)
              Node v l r ->
                let predecessor = findPredecessor l v
                in case predecessor of
                     Empty -> goLeft l ((v : go r prevAcc))
                     Node pv pl pr ->
                       if pr == Empty
                         then goLeft l ((v : go r (pv : prevAcc)))
                         else goLeft l ((v : go r prevAcc))
          findPredecessor start value =
            case start of
              Empty -> Empty
              Node v l r ->
                if r == Empty || r == Node value Empty Empty
                  then Node v l r
                  else findPredecessor r value
      in goLeft current acc