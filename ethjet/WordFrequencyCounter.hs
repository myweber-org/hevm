module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
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
    cleanWord = map toLower . filter isAlphaNum

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText contentmodule WordFrequencyCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a string
countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = groupCount wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    groupCount = foldr (\w acc -> case lookup w acc of
                                    Just count -> (w, count + 1) : filter ((/= w) . fst) acc
                                    Nothing -> (w, 1) : acc) []

-- | Get top N most frequent words
topWords :: Int -> String -> [WordCount]
topWords n text = take n $ countWords text

-- | Pretty print word frequencies
printFrequencies :: [WordCount] -> IO ()
printFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) countsmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.filter Char.isLetter . T.toLower

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) . Map.toList

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

processText :: T.Text -> Int -> [(T.Text, Int)]
processText text minFreq = sortByFrequency $ filterByMinFrequency minFreq $ countWords text

printResults :: [(T.Text, Int)] -> IO ()
printResults = mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " " <> word)

main :: IO ()
main = do
    content <- TIO.readFile "input.txt"
    let results = processText content 3
    printResults resultsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") freqs
    putStrLn "=========================="

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world world functional programming haskell"
    let frequencies = countWords sampleText
    printHistogram frequencies{-# LANGUAGE OverloadedStrings #-}

module WordFrequencyCounter where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

-- Common English stopwords
stopWords :: [T.Text]
stopWords = map T.pack ["the","a","an","and","or","but","in","on","at","to","for","of","with","by","as","is","was","were","be","been","am","are","this","that","these","those","i","you","he","she","it","we","they","my","your","his","her","its","our","their","me","him","us","them"]

-- Simple stemming: remove common suffixes
stemWord :: T.Text -> T.Text
stemWord word = foldl removeSuffix word suffixes
  where
    suffixes = ["ing", "ed", "s", "es", "ly", "ment", "ness", "ful", "less"]
    removeSuffix w suffix = 
        if suffix `T.isSuffixOf` w && T.length w > T.length suffix + 1
        then T.take (T.length w - T.length suffix) w
        else w

-- Clean and normalize text
normalizeText :: T.Text -> T.Text
normalizeText = T.toLower . T.filter (\c -> c == ' ' || c == '\'' || (c >= 'a' && c <= 'z'))

-- Count word frequencies with filtering and stemming
countWordFrequencies :: T.Text -> Map.Map T.Text Int
countWordFrequencies text = 
    let wordsList = T.words (normalizeText text)
        filteredWords = filter (\w -> not (T.null w) && w `notElem` stopWords) wordsList
        stemmedWords = map stemWord filteredWords
    in foldl (\acc w -> Map.insertWith (+) w 1 acc) Map.empty stemmedWords

-- Get top N frequent words
topNFrequentWords :: Int -> T.Text -> [(T.Text, Int)]
topNFrequentWords n text = 
    take n $ sortOn (Down . snd) $ Map.toList (countWordFrequencies text)

-- Process file and display results
processTextFile :: FilePath -> Int -> IO ()
processTextFile filePath n = do
    content <- TIO.readFile filePath
    let topWords = topNFrequentWords n content
    putStrLn $ "Top " ++ show n ++ " frequent words:"
    mapM_ (\(word, count) -> 
        TIO.putStrLn $ T.justifyLeft 15 ' ' word <> " : " <> T.pack (show count)) topWords

-- Example usage
main :: IO ()
main = do
    let sampleText = "The quick brown fox jumps over the lazy dog. The quick brown fox runs faster than the lazy dog."
    putStrLn "Sample text analysis:"
    print $ topNFrequentWords 5 sampleText