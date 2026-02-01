module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) . group . sort . words . normalize
  where
    normalize = map toLower . filter (\c -> not (c `elem` ",.!?;:\"()[]{}"))
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

displayCounts :: [(String, Int)] -> String
displayCounts = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: String -> String
processText = displayCounts . countWords