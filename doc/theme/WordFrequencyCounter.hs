module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let report = wordFrequencyReport sampleText 1 5
    
    putStrLn "Word Frequency Report:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) reportmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toLower . T.filter Char.isLetter

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortOn (negate . snd) . Map.toList

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

printWordFrequencies :: [(T.Text, Int)] -> IO ()
printWordFrequencies = mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " " <> word)

processText :: T.Text -> Int -> IO ()
processText text minFreq = do
    let counts = countWords text
    let filtered = filterByMinFrequency minFreq counts
    let sorted = sortByFrequency filtered
    printWordFrequencies sorted

main :: IO ()
main = do
    putStrLn "Enter text (end with Ctrl+D on empty line):"
    content <- TIO.getContents
    putStrLn "Enter minimum frequency:"
    minFreqInput <- getLine
    let minFreq = read minFreqInput :: Int
    processText content minFreqmodule WordFrequencyCounter where

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

printHistogram :: [WordCount] -> IO ()
printHistogram counts = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ printBar counts
  where
    printBar (word, count) = 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")"
    maxBarLength = 50

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    input <- getContents
    let frequencies = countWords input
    printHistogram frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

countWordFrequency :: String -> [(String, Int)]
countWordFrequency text =
  let wordsList = words text
      lowerWords = map (map toLower) wordsList
      sortedWords = sort lowerWords
      groupedWords = group sortedWords
      frequencies = map (\ws -> (head ws, length ws)) groupedWords
  in sortOn (Down . snd) frequencies

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
  unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> String
processText = displayFrequencies . countWordFrequencymodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

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
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    textPieces -> do
      let text = unwords textPieces
      let counts = countWords text
      putStrLn "Word frequencies:"
      putStr $ formatResults countsmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordFreq = Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

getTopFrequentWords :: Int -> WordFreq -> [(String, Int)]
getTopFrequentWords n freqMap = 
  take n $ sortOn (Down . snd) $ Map.toList freqMap

analyzeText :: String -> Int -> [(String, Int)]
analyzeText text n = getTopFrequentWords n $ countWords text

printAnalysis :: String -> Int -> IO ()
printAnalysis text n = do
  putStrLn $ "Top " ++ show n ++ " most frequent words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
        (analyzeText text n)module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort words'
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText contentmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        sortedWords = sort wordsList
        grouped = group sortedWords
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

wordFrequencyReport :: Int -> String -> [(String, Int)]
wordFrequencyReport n = getTopWords n . countWords

main :: IO ()
main = do
  let text = "Hello world hello Haskell world of functional programming"
  let topWords = wordFrequencyReport 3 text
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords

-- Example usage
sampleText :: String
sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis:"
    putStrLn $ wordFrequencyReport sampleText 2 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        counts = foldl incrementCount [] cleaned
    in sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha
    incrementCount [] word = [(word, 1)]
    incrementCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount rest word

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map toLower <$> wordsList
        frequencyMap = foldr countWord [] cleanedWords
    in sortOn (Down . snd) frequencyMap
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayTopWords :: Int -> [WordCount] -> String
displayTopWords n counts = 
    let topN = take n counts
        maxWordLength = maximum $ map (length . fst) topN
        formatLine (word, count) = 
            word ++ replicate (maxWordLength - length word + 2) ' ' ++ 
            "| " ++ show count
    in unlines $ map formatLine topN

processText :: String -> Int -> String
processText text n = 
    let counts = countWords text
    in "Top " ++ show n ++ " most frequent words:\n" ++ 
       displayTopWords n counts