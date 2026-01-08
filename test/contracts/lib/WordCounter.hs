
module WordCounter where

import Data.Char (isSpace)
import Data.List (group, sort)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = filter (not . all isSpace) $ splitWords text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        groupedWords = group sortedWords
    in map (\ws -> (head ws, length ws)) groupedWords
    where
        splitWords = words . map (\c -> if isSpace c then ' ' else c)
        toLower c
            | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
            | otherwise = c

displayWordCounts :: String -> IO ()
displayWordCounts text = do
    let counts = countWords text
    putStrLn "Word frequencies:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts
    putStrLn $ "Total unique words: " ++ show (length counts)