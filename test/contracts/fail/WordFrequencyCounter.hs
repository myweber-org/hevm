module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

countWordFrequency :: String -> [(String, Int)]
countWordFrequency text =
    let wordsList = words text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        grouped = group sortedWords
        frequencies = map (\ws -> (head ws, length ws)) grouped
        sortedFreq = sortOn (Down . snd) frequencies
    in sortedFreq

formatOutput :: [(String, Int)] -> String
formatOutput frequencies =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) frequencies

processText :: String -> String
processText = formatOutput . countWordFrequency

main :: IO ()
main = do
    input <- getContents
    putStr $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map Char.toLower . filter Char.isAlphaNum

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> putStrLn "Usage: wordfreq <text>"
    textPieces -> do
      let text = unwords textPieces
      putStrLn "Word frequencies:"
      putStrLn . formatResults $ countWords textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr (\w m -> insertWord w m) [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filename = do
    contents <- readFile filename
    let frequencies = countWords contents
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> processFile filenamemodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFreq = Map String Int

countWords :: String -> WordFreq
countWords = foldr updateFreq Map.empty . words
  where
    updateFreq word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: Int -> String -> String
processText n = displayFrequencies . getTopWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w acc -> case lookup w acc of
                                    Just count -> (w, count + 1) : filter ((/= w) . fst) acc
                                    Nothing -> (w, 1) : acc) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filepath = do
    content <- readFile filepath
    let frequencies = countWords content
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> processFile filepath
        _ -> putStrLn "Usage: wordfreq <filename>"
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower wordsList
        grouped = foldr countWord [] normalized
        sorted = sortOn (Down . snd) grouped
    in sorted
  where
    normalize = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
        uniqueWords = length counts
    in unlines $
        [ "Word Frequency Analysis Report"
        , "=============================="
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 10 most frequent words:"
        , "---------------------------"
        ] ++ map formatWordCount (take 10 counts)
  where
    formatWordCount (word, count) = word ++ ": " ++ show countmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr incrementWord [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlpha
    incrementWord word [] = [(word, 1)]
    incrementWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementWord word rest

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText text = 
    let topWords = countWords text
    in if null topWords 
        then "No words found in text"
        else "Top 10 most frequent words:\n" ++ formatResults topWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let normalized = map toLower text
        words' = words normalized
        cleanWords = filter (all isAlpha) words'
        frequencies = foldr countWord [] cleanWords
    in take 10 $ sortOn (Down . snd) frequencies
  where
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldl updateCount emptyMap wordsList
    in sortOn (Down . snd) $ toList freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    emptyMap = []
    updateCount [] word = [(word, 1)]
    updateCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateCount rest word
    toList = id

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun, world is big."
    let frequencies = countWords sampleText
    printWordFrequencies frequencies{-# LANGUAGE OverloadedStrings #-}

module WordFrequencyCounter where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

-- Common English stopwords
stopWords :: [T.Text]
stopWords = map T.pack ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "do", "does", "did", "will", "would", "shall", "should", "may", "might", "must", "can", "could", "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them"]

-- Simple stemming: remove common suffixes
stemWord :: T.Text -> T.Text
stemWord word
    | T.length word <= 3 = word
    | T.isSuffixOf "ing" word && T.length word > 5 = T.take (T.length word - 3) word
    | T.isSuffixOf "ed" word && T.length word > 4 = T.take (T.length word - 2) word
    | T.isSuffixOf "s" word && T.length word > 3 = T.take (T.length word - 1) word
    | T.isSuffixOf "ly" word && T.length word > 4 = T.take (T.length word - 2) word
    | otherwise = word

-- Clean and normalize a word
cleanWord :: T.Text -> Maybe T.Text
cleanWord txt = 
    let lower = T.toLower txt
        filtered = T.filter (\c -> isAlpha c || c == '\'') lower
        trimmed = T.strip filtered
    in if T.null trimmed || trimmed `elem` stopWords
        then Nothing
        else Just (stemWord trimmed)

-- Count word frequencies in text
countWordFrequencies :: T.Text -> Map.Map T.Text Int
countWordFrequencies text =
    let wordsList = T.words text
        cleanedWords = map cleanWord wordsList
        validWords = [w | Just w <- cleanedWords]
    in Map.fromListWith (+) [(w, 1) | w <- validWords]

-- Get top N frequent words
topNFrequentWords :: Int -> T.Text -> [(T.Text, Int)]
topNFrequentWords n text =
    let freqMap = countWordFrequencies text
        sorted = sortOn (Down . snd) (Map.toList freqMap)
    in take n sorted

-- Process a file and display top words
processFile :: FilePath -> Int -> IO ()
processFile filePath n = do
    content <- TIO.readFile filePath
    let topWords = topNFrequentWords n content
    putStrLn $ "Top " ++ show n ++ " frequent words in " ++ filePath ++ ":"
    mapM_ (\(word, count) -> 
        TIO.putStrLn $ T.justifyLeft 20 ' ' word <> " : " <> T.pack (show count)) topWords

-- Example usage in main
main :: IO ()
main = do
    let sampleText = "The quick brown fox jumps over the lazy dog. The dog barked at the fox, but the fox kept running. Running quickly, the fox escaped from the barking dog."
    putStrLn "Sample text analysis:"
    let topWords = topNFrequentWords 5 (T.pack sampleText)
    mapM_ (\(word, count) -> 
        TIO.putStrLn $ T.justifyLeft 15 ' ' word <> " : " <> T.pack (show count)) topWords
    
    -- To analyze a file, uncomment:
    -- processFile "document.txt" 10