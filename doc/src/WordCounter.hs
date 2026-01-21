module WordCounter where

import Data.Char (isSpace)
import Data.List (words)

countWords :: String -> Int
countWords input = length $ words input

countWordsStrict :: String -> Int
countWordsStrict = length . filter (not . all isSpace) . words