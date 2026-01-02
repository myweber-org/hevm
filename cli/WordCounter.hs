
module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . words

countUniqueWords :: String -> Int
countUniqueWords = length . groupBy (==) . words

wordFrequency :: String -> [(String, Int)]
wordFrequency = map (\ws -> (head ws, length ws)) . groupBy (==) . words

processText :: String -> String
processText text = unlines
    [ "Total words: " ++ show total
    , "Unique words: " ++ show unique
    , "Word frequencies:"
    , unlines (map (\(w, c) -> "  " ++ w ++ ": " ++ show c) freq)
    ]
    where
        total = countWords text
        unique = countUniqueWords text
        freq = wordFrequency text