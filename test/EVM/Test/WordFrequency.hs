module WordFrequency where

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

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords n
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth
    | maxWidth <= 0 = ""
    | otherwise = replicate barLength '█' ++ padding
  where
    barLength = round (fromIntegral count / fromIntegral maxWidth * 30)
    padding = replicate (30 - barLength) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    take 10 $ 
    sortOn (Down . snd) $
    map (\ws -> (head ws, length ws)) $
    group $
    sort $
    filter (not . null) $
    map (filter isAlpha . map toLower) $
    words text

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = do
    putStrLn "\nTop 10 Most Frequent Words:"
    putStrLn "============================="
    let maxCount = maximum (map snd counts)
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ " " ++ histogramBar count maxCount ++ " " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    let frequencies = analyzeText content
    displayHistogram frequenciesmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words . normalize
  where
    normalize = map Char.toLower . filter (\c -> Char.isAlpha c || Char.isSpace c)
    
    incrementWord word = Map.insertWith (+) word 1

topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . List.sortBy descending . Map.toList
  where
    descending (_, cnt1) (_, cnt2) = compare cnt2 cnt1

analyzeText :: String -> Int -> [(String, Int)]
analyzeText text n = topWords n (countWords text)

displayAnalysis :: String -> Int -> IO ()
displayAnalysis text n = do
  putStrLn $ "Top " ++ show n ++ " most frequent words:"
  mapM_ (\(word, count) -> putStrLn $ "  " ++ word ++ ": " ++ show count) 
        (analyzeText text n)module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords nmodule WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w m = Map.insertWith (+) (normalize w) 1 m
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n = take n . sortOn (negate . snd) . Map.toList . countWords

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word Frequency Analysis:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (topWords 10 text)module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlphaNum
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nTop 10 most frequent words:"
    putStrLn $ displayCounts frequencies