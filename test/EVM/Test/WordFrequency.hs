module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr (\word -> insertWord word) [] wordsList
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatEntry counts
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText text = displayResults $ countWords text