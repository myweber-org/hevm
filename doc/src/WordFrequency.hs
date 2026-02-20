module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr countHelper [] cleaned
        sorted = sortOn (Down . snd) grouped
    in sorted
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatResults :: [WordCount] -> String
formatResults counts =
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText = formatResults . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)

countWords :: String -> Map.Map String Int
countWords text = Map.fromListWith (+) [(word, 1) | word <- words text, not (null word)]
  where
    words = map (map toLower . filter isAlpha) . splitBySpace
    splitBySpace = foldr splitter [[]]
    splitter c acc@(current:rest)
      | c == ' '  = []:acc
      | otherwise = (c:current):rest

mostFrequent :: String -> Maybe (String, Int)
mostFrequent text
  | Map.null freqMap = Nothing
  | otherwise = Just (Map.foldrWithKey maxEntry ("", 0) freqMap)
  where
    freqMap = countWords text
    maxEntry word count (maxWord, maxCount)
      | count > maxCount = (word, count)
      | otherwise = (maxWord, maxCount)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    groupCount [] = []
    groupCount (x:xs) = 
        let (matches, rest) = partition (== x) xs
            count = 1 + length matches
        in (x, count) : groupCount rest

    partition _ [] = ([], [])
    partition p (y:ys)
        | p y       = let (as, bs) = partition p ys in (y:as, bs)
        | otherwise = let (as, bs) = partition p ys in (as, y:bs)

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    let frequencies = countWords sampleText
    printWordCounts frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w m = Map.insertWith (+) (normalize w) 1 m
    normalize = map toLower . filter isAlpha

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

displayResults :: [(String, Int)] -> String
displayResults = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: String -> Int -> String
processText text n = displayResults $ getTopWords n $ countWords textmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: WordCount -> String
displayFrequency counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = displayFrequency . countWordsmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)

countWords :: String -> Map.Map String Int
countWords text = Map.fromListWith (+) wordCounts
  where
    words' = map (map toLower) $ filter (all isAlpha) $ words text
    wordCounts = zip words' (repeat 1)

mostFrequent :: String -> [(String, Int)]
mostFrequent text = take 5 $ reverse $ sortByFrequency $ Map.toList $ countWords text
  where
    sortByFrequency = sortOn sndmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text = foldr incrementWord Map.empty (words text)
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  let topWords = getTopWords 10 content
  putStrLn "Top 10 most frequent words:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalize = map toLower . filter isAlpha
        frequencies = foldr countWord [] wordsList
        countWord w [] = [(w, 1)]
        countWord w ((word, count):rest)
            | w == word = (word, count + 1) : rest
            | otherwise = (word, count) : countWord w rest
    in sortOn (Down . snd) frequencies

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> do
            content <- readFile filename
            let frequencies = countWords content
            putStrLn $ formatOutput frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: WordCount -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let frequencies = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency frequenciesmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topNWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
    putStrLn $ "Top " ++ show n ++ " most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (topNWords n text)

sampleText :: String
sampleText = "Hello world! Hello Haskell! Haskell is great. World says hello back."