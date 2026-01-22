module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = displayFrequency . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = unlines $ map showEntry counts
  where
    showEntry (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText text = 
    let frequencies = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency frequencies