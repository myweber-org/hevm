module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr incrementWord [] wordsList
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha
    incrementWord word [] = [(word, 1)]
    incrementWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatEntry counts
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords