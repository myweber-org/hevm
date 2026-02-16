
module BinaryTree where

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | x > y     = Node y left (insert x right)
    | otherwise = Node y left right

inOrder :: BinaryTree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

preOrder :: BinaryTree a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

postOrder :: BinaryTree a -> [a]
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]
module BinaryTree where

import Data.Aeson (ToJSON, toJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance ToJSON a => ToJSON (Tree a) where
    toJSON Empty = toJSON ([] :: [String])
    toJSON (Node val left right) = toJSON [toJSON val, toJSON left, toJSON right]

serializeTree :: ToJSON a => Tree a -> String
serializeTree = unpack . encode

sampleTree :: Tree Int
sampleTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)