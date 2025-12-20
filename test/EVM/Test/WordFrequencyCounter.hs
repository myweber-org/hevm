module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldl (\acc word -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

displayFrequencies :: [WordFreq] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> String
processText = displayFrequencies . countWordFrequencies