module BinaryTreeSerializer where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

serialize :: (Show a) => BinaryTree a -> String
serialize Empty = "# "
serialize (Node val left right) = show val ++ " " ++ serialize left ++ serialize right

deserialize :: (Read a) => String -> BinaryTree a
deserialize = fst . build . words
  where
    build ("#" : xs) = (Empty, xs)
    build (x : xs) =
      let (left, rest1) = build xs
          (right, rest2) = build rest1
       in (Node (read x) left right, rest2)
    build [] = (Empty, [])