
module WordFrequency where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: [String] -> WordCount
countWords = foldr incrementWord Map.empty . concatMap words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

topWords :: Int -> [String] -> [(String, Int)]
topWords n texts = take n $ sortOn (negate . snd) $ Map.toList $ countWords texts

displayFrequency :: [(String, Int)] -> String
displayFrequency counts = unlines $ map format counts
  where
    format (word, count) = word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    groupCount [] = []
    groupCount (x:xs) = 
        let (matches, rest) = partition (== x) (x:xs)
        in (x, length matches) : groupCount rest

    partition _ [] = ([], [])
    partition p (y:ys)
        | p y       = let (as, bs) = partition p ys in (y:as, bs)
        | otherwise = let (as, bs) = partition p ys in (as, y:bs)

printResults :: [WordCount] -> IO ()
printResults counts = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Reading from stdin..."
            content <- getContents
            printResults $ countWords content
        [filename] -> do
            content <- readFile filename
            printResults $ countWords content
        _ -> putStrLn "Usage: wordfreq [filename] (reads from stdin if no filename)"module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a string
countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

-- | Normalize word: lowercase and keep only alphabetic characters
normalize :: String -> String
normalize = map toLower . filter isAlpha

-- | Sort word counts by frequency (descending)
sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

-- | Filter words with frequency above threshold
filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

-- | Get top N most frequent words
topNWords :: Int -> [WordCount] -> [WordCount]
topNWords n = take n . sortByFrequency

-- | Complete analysis pipeline
analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    topNWords topN 
    . filterByMinFrequency minFreq 
    . sortByFrequency 
    $ countWords text

-- | Pretty print word frequencies
printFrequencies :: [WordCount] -> IO ()
printFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) countsmodule WordFrequency where

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

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStr $ processText inputmodule WordFrequency where

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

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords nmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        frequencyMap = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let results = countWords input
    putStrLn $ displayResults resultsmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = groupCount cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    groupCount [] = []
    groupCount (w:ws) = 
        let (matches, rest) = partition (== w) ws
        in (w, 1 + length matches) : groupCount rest

    partition _ [] = ([], [])
    partition p (x:xs)
        | p x       = (x:ys, zs)
        | otherwise = (ys, x:zs)
      where (ys, zs) = partition p xs