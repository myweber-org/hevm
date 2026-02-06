module WordCounter where

import Data.Char (isSpace, isPunctuation)
import Data.List (group, sort)

cleanText :: String -> String
cleanText = map toLower . filter (not . isPunctuation)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = words $ cleanText text
        sortedWords = sort wordsList
        groupedWords = group sortedWords
    in map (\ws -> (head ws, length ws)) groupedWords

mostFrequentWords :: Int -> String -> [(String, Int)]
mostFrequentWords n text = 
    take n $ reverse $ sortOn snd (countWords text)

wordFrequency :: String -> String -> Maybe Int
wordFrequency word text = 
    lookup (map toLower word) (countWords text)

processText :: String -> IO ()
processText inputText = do
    putStrLn "Word frequencies:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (countWords inputText)
    
    putStrLn "\nTop 5 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (mostFrequentWords 5 inputText)