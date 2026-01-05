module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts

filterByFrequency :: Int -> [WordCount] -> [WordCount]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

displayWordCounts :: [WordCount] -> String
displayWordCounts counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> Int -> String
processText text minFrequency = 
    let counts = countWords text
        filtered = filterByFrequency minFrequency counts
    in displayWordCounts filtered