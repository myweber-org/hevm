
module WordCounter where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords = length . words

countCharacters :: String -> Int
countCharacters = length

countLines :: String -> Int
countLines = length . lines

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

wordFrequency :: String -> [(String, Int)]
wordFrequency text = map (\ws -> (head ws, length ws)) $ group $ sort $ words processed
  where
    processed = map toLower $ filter (\c -> isAlpha c || isSpace c) text
    group = foldr (\x acc -> case acc of
                              [] -> [[x]]
                              (y:ys):yss -> if x == y then (x:y:ys):yss else [x]:(y:ys):yss) []
    sort = foldr insert []
    insert x [] = [x]
    insert x (y:ys) = if x <= y then x:y:ys else y:insert x ys

processText :: String -> (Int, Int, Int, [(String, Int)])
processText text = (countWords text, countCharacters text, countLines text, wordFrequency text)