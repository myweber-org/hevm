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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topNWords n
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import Control.Arrow ((&&&))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth
    | maxWidth <= 0 = ""
    | otherwise = replicate barLength '█' ++ padding
  where
    scale = fromIntegral maxWidth / 50.0
    barLength = round (fromIntegral count / scale)
    padding = replicate (maxWidth - barLength) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    take 10 $ 
    sortOn (Down . snd) $
    map (head &&& length) $
    group $
    sort $
    filter (not . null) $
    map normalize $
    words text
  where
    normalize = filter isAlpha . map toLower

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = do
    putStrLn "\nTop 10 Most Frequent Words:"
    putStrLn "============================"
    
    let maxCount = maximum (map snd counts)
        maxWordLength = maximum (map (length . fst) counts)
    
    mapM_ (\(word, count) -> do
        let paddedWord = word ++ replicate (maxWordLength - length word + 2) ' '
            bar = histogramBar count maxCount
        putStrLn $ paddedWord ++ bar ++ " " ++ show count
        ) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    let frequencies = analyzeText content
    displayHistogram frequenciesmodule WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        freqMap = foldr (\w m -> insertWord w m) [] cleaned
    in sortOn (Down . snd) freqMap
  where
    cleanWord = filter isAlpha
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

displayTopWords :: Int -> String -> String
displayTopWords n text = 
    let counts = take n $ countWords text
    in unlines $ map (\(w, c) -> w ++ ": " ++ show c) countsmodule WordFrequency where

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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords n