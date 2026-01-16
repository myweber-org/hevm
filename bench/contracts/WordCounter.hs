
module WordCounter where

import Data.Char (isSpace)
import Data.List (groupBy)

countWords :: String -> Int
countWords = length . words

countUniqueWords :: String -> Int
countUniqueWords = length . groupBy sameWord . words
  where sameWord a b = map toLower a == map toLower b
        toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

getWordFrequencies :: String -> [(String, Int)]
getWordFrequencies text = map (\ws -> (head ws, length ws)) grouped
  where
    lowerWords = map (map toLower) (words text)
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    grouped = groupBy (==) (sort lowerWords)

processText :: String -> IO ()
processText text = do
  putStrLn $ "Total words: " ++ show (countWords text)
  putStrLn $ "Unique words: " ++ show (countUniqueWords text)
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) (getWordFrequencies text)