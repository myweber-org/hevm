module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        frequencies = foldr countWord [] cleaned
    in take 10 $ sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequency :: WordCount -> String
displayFrequency freq = unlines $ map (\(w, c) -> w ++ ": " ++ show c) freq

analyzeText :: String -> String
analyzeText text = 
    let freq = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency freq