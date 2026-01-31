
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

wordFrequency :: String -> IO ()
wordFrequency input = do
    let counts = countWords input
    putStrLn "Word frequencies:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts