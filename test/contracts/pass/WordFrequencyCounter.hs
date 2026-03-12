module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = words text
        lowerWords = map (map toLower) words'
        sortedWords = sort lowerWords
        grouped = group sortedWords
        counts = map (\ws -> (head ws, length ws)) grouped
        sortedCounts = sortOn (Down . snd) counts
    in sortedCounts

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    let allCounts = countWords text
        filtered = filterByMinFrequency minFreq allCounts
    in getTopNWords topN filtered

main :: IO ()
main = do
    let sampleText = "Hello world hello Haskell world of functional programming"
    let result = analyzeText sampleText 1 5
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) resultmodule WordFrequencyCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortBy)
import Data.Ord (comparing)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleanedWords
    in sortBy (comparing (negate . snd)) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

getTopWords :: Int -> String -> [WordCount]
getTopWords n text = take n $ countWords text

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map formatPair counts
  where
    formatPair (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText text = formatResults $ getTopWords 10 textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = group $ sort cleaned
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN = 
    getTopNWords topN $ filterByMinFrequency minFreq $ countWords text

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let report = wordFrequencyReport sampleText 1 5
    
    putStrLn "Word Frequency Report:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) report