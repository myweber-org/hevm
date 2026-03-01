module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordFreq]
analyzeText text minFreq topN = 
    getTopNWords topN . filterByMinFrequency minFreq . countWords $ text

displayResults :: [WordFreq] -> IO ()
displayResults freqs = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqs
    putStrLn $ "Total unique words: " ++ show (length freqs)

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    let results = analyzeText sampleText 1 5
    displayResults resultsmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        grouped = group sortedWords
        counts = map (\ws -> (head ws, length ws)) grouped
        sortedCounts = sortOn (Down . snd) counts
    in sortedCounts

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

displayWordCounts :: [WordCount] -> String
displayWordCounts counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> Int -> Int -> String
processText text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
        topWords = getTopNWords topN filtered
    in displayWordCounts topWords