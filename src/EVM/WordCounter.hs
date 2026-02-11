module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) . group . sort . words

countUniqueWords :: String -> Int
countUniqueWords = length . countWords

getMostFrequentWords :: Int -> String -> [(String, Int)]
getMostFrequentWords n = take n . reverse . sortOn snd . countWords

cleanText :: String -> String
cleanText = map toLower . filter (\c -> not (c `elem` ",.!?;:\"\'()[]{}"))

processText :: String -> [(String, Int)]
processText = countWords . cleanText

displayResults :: [(String, Int)] -> IO ()
displayResults = mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count)