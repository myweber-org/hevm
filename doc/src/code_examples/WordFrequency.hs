
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr (\w m -> insertWith (+) w 1 m) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    insertWith f key value [] = [(key, value)]
    insertWith f key value ((k,v):rest)
        | key == k  = (k, f v value) : rest
        | otherwise = (k,v) : insertWith f key value rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w,c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> do
            content <- readFile filename
            putStrLn $ formatOutput $ countWords contentmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words . map normalize
  where
    normalize c
      | isAlpha c = toLower c
      | otherwise = ' '
    
    incrementWord word = Map.insertWith (+) word 1

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl (\acc word -> insertWord word acc) [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

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
       displayTopWords n countsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords text = foldr incrementWord Map.empty words
  where
    words' = map normalize $ filter (not . null) $ splitWords text
    words = filter (all isAlpha) words'
    
    normalize = map toLower
    splitWords = words . map (\c -> if c `elem` ".,!?;:" then ' ' else c)
    
    incrementWord word acc = Map.insertWith (+) word 1 acc

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (\(_, count) -> negate count) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs = unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

analyzeText :: Int -> String -> String
analyzeText n text = displayFrequencies $ topNWords n text