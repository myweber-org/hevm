module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

-- | Count word frequencies in a text string
countWords :: String -> WordCount
countWords text = Map.fromListWith (+) [(w, 1) | w <- words (normalizeText text)]

-- | Normalize text by converting to lowercase and removing non-alphabetic characters
normalizeText :: String -> String
normalizeText = unwords . map (filter isAlpha . map toLower) . words

-- | Get top N most frequent words
topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (\(_, count) -> negate count) . Map.toList

-- | Pretty print word frequencies
printWordFrequencies :: [(String, Int)] -> String
printWordFrequencies freqs = unlines [word ++ ": " ++ show count | (word, count) <- freqs]

-- | Process a text and return top N words
analyzeText :: Int -> String -> String
analyzeText n text = printWordFrequencies $ topNWords n (countWords text)module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

-- | Count frequency of each word in a text string
countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalizeWord $ words text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    normalizeWord = map toLower . filter isAlpha
    
    insertWord :: String -> [WordFreq] -> [WordFreq]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

-- | Get top N most frequent words
topNWords :: Int -> String -> [WordFreq]
topNWords n text = take n $ countWordFrequencies text

-- | Calculate word frequency distribution as percentages
frequencyDistribution :: String -> [(String, Double)]
frequencyDistribution text =
    let freqs = countWordFrequencies text
        total = fromIntegral $ sum $ map snd freqs
    in map (\(w, c) -> (w, (fromIntegral c / total) * 100)) freqs

-- | Filter words by minimum frequency threshold
filterByMinFrequency :: Int -> String -> [WordFreq]
filterByMinFrequency minFreq text =
    filter (\(_, count) -> count >= minFreq) $ countWordFrequencies text
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
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

formatOutput :: [WordFreq] -> String
formatOutput freqs = unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> Int -> Int -> String
processText text minFreq topN = 
    let freqs = countWords text
        filtered = filterByFrequency minFreq freqs
        topWords = getTopNWords topN filtered
    in formatOutput topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    putStrLn "Word Frequency Analysis:"
    putStrLn $ processText sampleText 1 5{-# LANGUAGE OverloadedStrings #-}

module WordFrequencyCounter where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

-- Common English stopwords
stopWords :: [T.Text]
stopWords = map T.pack ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "do", "does", "did", "will", "would", "shall", "should", "may", "might", "must", "can", "could", "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them"]

-- Simple stemming: remove common suffixes
stemWord :: T.Text -> T.Text
stemWord word = foldl removeSuffix word suffixes
  where
    suffixes = ["ing", "ed", "s", "es", "ly", "ment", "ness", "ful", "less", "able", "ible"]
    removeSuffix w suffix = if T.isSuffixOf suffix w 
                            then T.take (T.length w - T.length suffix) w 
                            else w

-- Clean and normalize text
cleanText :: T.Text -> T.Text
cleanText = T.toLower . T.filter (\c -> c == ' ' || c == '\'' || (c >= 'a' && c <= 'z'))

-- Count word frequencies with stemming and stopword filtering
countWordFrequencies :: T.Text -> Map.Map T.Text Int
countWordFrequencies text = 
    let cleaned = cleanText text
        wordsList = filter (not . T.null) $ T.split (== ' ') cleaned
        filteredWords = filter (\w -> not (w `elem` stopWords)) wordsList
        stemmedWords = map stemWord filteredWords
    in foldl (\acc w -> Map.insertWith (+) w 1 acc) Map.empty stemmedWords

-- Get top N most frequent words
topNFrequentWords :: Int -> T.Text -> [(T.Text, Int)]
topNFrequentWords n text = 
    take n $ sortOn (Down . snd) $ Map.toList $ countWordFrequencies text

-- Process a text file and display top words
processFile :: FilePath -> Int -> IO ()
processFile filepath n = do
    content <- TIO.readFile filepath
    let topWords = topNFrequentWords n content
    putStrLn $ "Top " ++ show n ++ " most frequent words:"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.concat [word, ": ", T.pack (show count)]) topWords

-- Example usage in main
main :: IO ()
main = do
    let sampleText = "The quick brown fox jumps over the lazy dog. The quick brown fox runs fast. A quick brown fox is always faster than a lazy dog."
    putStrLn "Analyzing sample text..."
    let frequencies = countWordFrequencies $ T.pack sampleText
    print frequencies
    
    putStrLn "\nTop 5 words:"
    let top5 = topNFrequentWords 5 $ T.pack sampleText
    print top5module WordFrequencyCounter where

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
getTopNWords n = take n

processText :: String -> Int -> Int -> [WordFreq]
processText text minFreq topN = 
    let counted = countWords text
        filtered = filterByMinFrequency minFreq counted
        sorted = sortByFrequency filtered
    in getTopNWords topN sorted

displayResults :: [WordFreq] -> IO ()
displayResults freqList = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqList

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World says hello."
    let results = processText sampleText 1 5
    putStrLn "Top 5 most frequent words (minimum frequency: 1):"
    displayResults results