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

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minFreq topN =
    let freqs = countWords text
        filtered = filterByMinFrequency minFreq freqs
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords

exampleUsage :: IO ()
exampleUsage = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    putStrLn "Word frequency analysis:"
    putStrLn $ wordFrequencyReport sampleText 1 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countWord [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord w [] = [(w, 1)]
    countWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : countWord w rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = filter isAlpha . map toLower
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = 
    take n $ sortOn (Down . snd) $ Map.toList freqMap

filterByMinimum :: Int -> WordFrequency -> WordFrequency
filterByMinimum minCount = Map.filter (>= minCount)

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text topN minCount = do
    let frequencies = countWords text
    let filtered = filterByMinimum minCount frequencies
    let topWords = getTopWords topN filtered
    
    putStrLn "Top words by frequency:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size frequencies)
    putStrLn $ "Words meeting minimum count (" ++ show minCount ++ "): " ++ show (Map.size filtered)

sampleText :: String
sampleText = 
    "This is a sample text. This text contains repeated words. " ++
    "Words like this and text appear multiple times. " ++
    "Sample text analysis helps understand word frequency patterns."

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis"
    putStrLn "======================="
    analyzeText sampleText 5 2module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

countWords :: String -> Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = 
    take n $ sortOn (\(_, count) -> negate count) $ Map.toList $ countWords text

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
    putStrLn $ "Top " ++ show n ++ " most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) $ getTopWords n text

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again, world. Testing word frequency."
    displayTopWords 5 sampleText