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

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topNWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: [String] -> FrequencyMap
countWords = foldr (\word -> Map.insertWith (+) word 1) Map.empty

getTopFrequencies :: Int -> FrequencyMap -> [(String, Int)]
getTopFrequencies n freqMap = 
    take n $ sortOn (Down . snd) $ Map.toList freqMap

analyzeText :: String -> Int -> [(String, Int)]
analyzeText text n = 
    let wordsList = words text
        freqMap = countWords wordsList
    in getTopFrequencies n freqMap

displayAnalysis :: String -> Int -> IO ()
displayAnalysis text n = do
    putStrLn $ "Top " ++ show n ++ " most frequent words:"
    mapM_ (\(word, count) -> 
        putStrLn $ "  " ++ word ++ ": " ++ show count) 
        (analyzeText text n)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWords