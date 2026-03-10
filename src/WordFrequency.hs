module WordFrequency where

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
                Nothing -> (word, 1) : acc
            ) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World says hello."
    let frequencies = countWords sampleText
    printWordFrequencies frequenciesmodule WordFrequency where

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

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFreq = Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (\(_, freq) -> -freq) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(word, freq) -> word ++ ": " ++ show freq)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' (\acc w -> Map.insertWith (+) w 1 acc) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, a) (_, b) -> compare b a) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords nmodule WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        freqMap = foldr (\w m -> insertWord w m) [] cleaned
        sorted = sortOn (Down . snd) freqMap
    in sorted
  where
    cleanWord = filter isAlpha
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

mostFrequentWords :: Int -> String -> [WordCount]
mostFrequentWords n text = take n $ countWords text

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts