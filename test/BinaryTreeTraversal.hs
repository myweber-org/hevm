
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
               result -> resultdata Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node val left right) = inOrder left ++ [val] ++ inOrder right

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node val left right) = [val] ++ preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node val left right) = postOrder left ++ postOrder right ++ [val]

exampleTree :: Tree Int
exampleTree = Node 1
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 5 Empty Empty))
                (Node 3
                    (Node 6 Empty Empty)
                    Empty)