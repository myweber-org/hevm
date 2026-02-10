
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) 
           . group 
           . sort 
           . words 
           . map toLower
           . filter (\c -> not (isSpace c) || c == ' ')

toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

mostFrequent :: String -> (String, Int)
mostFrequent text = 
    case countWords text of
        [] -> ("", 0)
        ws -> maximumBy (compare `on` snd) ws

uniqueWords :: String -> Int
uniqueWords = length . countWords