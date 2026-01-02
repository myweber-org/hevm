module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower wordsList
        grouped = group $ sort normalized
    in map (\ws -> (head ws, length ws)) grouped

normalize :: String -> String
normalize = filter isAlpha

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopWords :: Int -> [WordCount] -> [WordCount]
getTopWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    getTopWords topN . filterByMinFrequency minFreq $ countWords text

displayResults :: [WordCount] -> IO ()
displayResults counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let results = analyzeText sampleText 1 5
    putStrLn "Top 5 most frequent words (appearing at least once):"
    displayResults resultsmodule WordFrequency where

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

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> Int -> Int -> String
analyzeText text minFreq topN = 
    let freqList = countWords text
        filtered = filterByMinFrequency minFreq freqList
        topWords = getTopNWords topN filtered
    in formatResults topWords