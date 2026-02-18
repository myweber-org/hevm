module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord $ topWords 10 text
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

countWords :: String -> Histogram
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: Histogram -> IO ()
printHistogram hist = mapM_ printEntry hist
  where
    printEntry (word, count) = putStrLn $ word ++ ": " ++ replicate count '*'module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = filter isAlpha . map toLower
        frequencyMap = foldr incrementWord [] wordsList
        incrementWord word [] = [(word, 1)]
        incrementWord word ((w, c):rest)
            | w == word = (w, c + 1) : rest
            | otherwise = (w, c) : incrementWord word rest
    in sortOn (Down . snd) frequencyMap

topNWords :: Int -> String -> WordCount
topNWords n text = take n $ countWords text

displayFrequency :: WordCount -> String
displayFrequency counts = unlines $ map showEntry counts
  where
    showEntry (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText text = 
    let totalWords = length $ words text
        uniqueWords = length $ countWords text
        topWords = take 5 $ countWords text
    in unlines
        [ "Text Analysis Results:"
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , "Top 5 frequent words:"
        , displayFrequency topWords
        ]module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. World of functional programming."
    let frequencies = countWords sampleText
    printWordFrequencies frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

analyzeText :: String -> IO ()
analyzeText input = do
    let topWords = countWords input
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ topWords 10 text