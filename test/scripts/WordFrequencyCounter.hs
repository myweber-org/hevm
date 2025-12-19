module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
    in aggregateCounts cleanedWords
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

aggregateCounts :: [String] -> [WordCount]
aggregateCounts = foldr incrementCount []
  where
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word rest

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStr $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        grouped = foldr countHelper [] cleaned
        sorted = sortOn (Down . snd) grouped
    in sorted
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText input