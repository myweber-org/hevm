module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl updateCount [] cleanedWords
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    updateCount [] word = [(word, 1)]
    updateCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateCount rest word

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let frequencies = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency frequencies