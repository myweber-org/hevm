module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl (\acc w -> insertWord w acc) [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')

insertWord :: String -> [WordCount] -> [WordCount]
insertWord word [] = [(word, 1)]
insertWord word ((w, c):rest)
    | w == word = (w, c + 1) : rest
    | otherwise = (w, c) : insertWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatResults frequencies