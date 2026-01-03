
module BinaryTreeTraversal where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

inorderMorris :: BinaryTree a -> [a]
inorderMorris Empty = []
inorderMorris root = go root []
  where
    go Empty acc = acc
    go (Node val left right) acc =
      let current = left
          process node result =
            case node of
              Empty -> result
              Node v l r ->
                let predecessor = findPredecessor l v
                 in case predecessor of
                      Nothing -> process l (v : process r result)
                      Just (predVal, predRight) ->
                        if predRight == node
                          then process l (v : process r result)
                          else process l (predVal : process r result)
       in process current (val : go right acc)

    findPredecessor :: BinaryTree a -> a -> Maybe (a, BinaryTree a)
    findPredecessor Empty _ = Nothing
    findPredecessor (Node v l r) target =
      if r == target
        then Just (v, r)
        else case findPredecessor l target of
               Nothing -> findPredecessor r target
               result -> result