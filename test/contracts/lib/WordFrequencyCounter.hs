module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] wordsList
    in sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter isAlpha

sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordFreq] -> [WordFreq]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n . sortByFrequency

formatOutput :: [WordFreq] -> String
formatOutput = unlines . map (\(word, count) -> word ++ ": " ++ show count)

analyzeText :: String -> Int -> Int -> String
analyzeText text minFreq topN =
    let freqList = countWords text
        filtered = filterByMinFrequency minFreq freqList
        topWords = getTopNWords topN filtered
    in formatOutput topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."
    putStrLn "Word frequency analysis:"
    putStrLn $ analyzeText sampleText 1 5module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words $ map normalizeChar text
        normalized = map normalizeWord wordsList
        filtered = filter (not . isStopWord) normalized
        grouped = group $ sort filtered
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    normalizeChar c
        | c `elem` ",.!?;:\"()[]{}" = ' '
        | otherwise = c
    
    normalizeWord = map toLower
    
    isStopWord w = w `elem` commonStopWords
    
    commonStopWords = ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by"]

filterByMinimumFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinimumFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."
    let frequencies = countWords sampleText
    
    putStrLn "All word frequencies:"
    printWordFrequencies frequencies
    
    putStrLn "\nWords with frequency >= 2:"
    printWordFrequencies $ filterByMinimumFrequency 2 frequencies
    
    putStrLn "\nTop 3 words:"
    printWordFrequencies $ getTopNWords 3 frequencies