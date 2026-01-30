module WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlphaNum

printFrequencies :: Map.Map String Int -> IO ()
printFrequencies freqMap = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (Map.toList freqMap)

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun, isn't it?"
    let frequencies = countWords sampleText
    printFrequencies frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter (\c -> isAlphaNum c || c == '\'')

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
    in formatOutput topWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFrequency = Map String Int

countWords :: String -> WordFrequency
countWords text = foldr incrementWord Map.empty (words processedText)
  where
    processedText = map normalizeChar text
    normalizeChar c
        | isAlpha c = toLower c
        | otherwise = ' '
    
    incrementWord word freqMap = Map.insertWith (+) word 1 freqMap

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = take n $ sortOn (\(_, count) -> negate count) $ Map.toList freqMap

analyzeText :: String -> Int -> [(String, Int)]
analyzeText text n = getTopWords n $ countWords text

displayAnalysis :: String -> Int -> IO ()
displayAnalysis text n = do
    putStrLn "Top words by frequency:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
  where
    topWords = analyzeText text n

-- Example usage
sampleText :: String
sampleText = "Hello world! Hello Haskell. Haskell is functional. World of Haskell."

main :: IO ()
main = displayAnalysis sampleText 3module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

displayWordCounts :: [WordCount] -> IO ()
displayWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

sampleAnalysis :: IO ()
sampleAnalysis = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World says hello."
    putStrLn "Word frequency analysis:"
    displayWordCounts $ wordFrequencyReport sampleText 1 5