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
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ topWords 10 textmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map (map toLower) wordsList
        grouped = group $ sort cleaned
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    clean = filter isAlpha

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    getTopNWords topN $ filterByMinFrequency minFreq $ countWords text

displayResults :: [WordCount] -> IO ()
displayResults counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World of Haskell."
    putStrLn "Word frequency analysis:"
    displayResults $ analyzeText sampleText 1 5module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count