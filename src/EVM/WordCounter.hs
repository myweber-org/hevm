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
displayResults = mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count)module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . filter (not . all isSpace) . groupBy (\x y -> not (isSpace x && isSpace y))

processText :: String -> String
processText text = "Word count: " ++ show (countWords text) ++ "\n" ++ text

main :: IO ()
main = do
    putStrLn "Enter text to count words:"
    input <- getContents
    putStrLn $ processText input