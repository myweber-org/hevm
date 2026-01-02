module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) . group . sort . words . filter (not . isSpace)

displayCounts :: [(String, Int)] -> String
displayCounts = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: String -> String
processText = displayCounts . countWords