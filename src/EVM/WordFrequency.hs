module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        wordMap = foldr (\word -> insertWith (+) word 1) [] cleanedWords
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    insertWith :: (Int -> Int -> Int) -> String -> Int -> [WordCount] -> [WordCount]
    insertWith f key value [] = [(key, value)]
    insertWith f key value ((k,v):xs)
        | key == k  = (k, f v value) : xs
        | otherwise = (k,v) : insertWith f key value xs

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWords