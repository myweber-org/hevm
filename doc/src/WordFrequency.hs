
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

analyzeText :: String -> [WordCount]
analyzeText text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
        sortedCounts = sortOn (Down . snd) counts
    in sortedCounts
  where
    cleanWord = filter isAlpha

filterByMinimum :: Int -> [WordCount] -> [WordCount]
filterByMinimum minCount = filter (\(_, count) -> count >= minCount)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minCount topN =
    let analysis = analyzeText text
        filtered = filterByMinimum minCount analysis
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords