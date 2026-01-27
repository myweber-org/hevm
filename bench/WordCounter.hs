
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) 
           . group 
           . sort 
           . words
           . map normalizeChar
  where
    normalizeChar c
      | c `elem` ".,!?;:\"" = ' '
      | otherwise = toLower c
    toLower = toEnum . (+32) . fromEnum

displayCounts :: [(String, Int)] -> String
displayCounts = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: String -> String
processText = displayCounts . countWords