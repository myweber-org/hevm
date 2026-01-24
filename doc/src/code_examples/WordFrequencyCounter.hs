module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minFreq = filter (\(_, freq) -> freq >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

printFrequencies :: [WordFreq] -> IO ()
printFrequencies freqs = 
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) freqs

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text minFreq topN = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    let allFreqs = countWords text
        filtered = filterByFrequency minFreq allFreqs
        topWords = getTopNWords topN filtered
    printFrequencies topWords
    putStrLn $ "Total unique words: " ++ show (length allFreqs)
    putStrLn $ "Words with frequency >= " ++ show minFreq ++ ": " ++ show (length filtered)

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog was not amused by the fox."

main :: IO ()
main = analyzeText sampleText 2 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl countWord [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordCount] -> [WordCount]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> Int -> Int -> String
processText text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
        topWords = getTopNWords topN filtered
    in formatOutput topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."
    putStrLn "Word frequency analysis:"
    putStrLn $ processText sampleText 1 5