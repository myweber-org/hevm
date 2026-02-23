
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
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = filter (not . null) $ splitWords text
        sortedWords = sort $ map normalize wordsList
        grouped = group sortedWords
    in map (\ws -> (head ws, length ws)) grouped
    where
        splitWords = words . map (\c -> if isSpace c then ' ' else c)
        normalize = map toLower . filter (not . isPunctuation)
        isPunctuation c = c `elem` ".,!?;:\"'()[]{}"
        toLower c
            | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
            | otherwise = c