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

main :: IO ()
main = do
    putStrLn "Enter some text (press Ctrl+D to end input on Unix/Linux, Ctrl+Z on Windows):"
    input <- getContents
    let cleaned = trim input
    putStrLn $ "Word count: " ++ show (countWords cleaned)
    putStrLn $ "Character count: " ++ show (countCharacters cleaned)
    putStrLn $ "Line count: " ++ show (countLines cleaned)
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = filter (not . all isSpace) $ splitWords text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        grouped = group sortedWords
    in map (\ws -> (head ws, length ws)) grouped
  where
    splitWords = words . map (\c -> if isSpace c then ' ' else c)
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c