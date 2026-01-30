module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr insertWord Map.empty . words
  where
    insertWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word Frequency Analysis:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
        (topWords 10 text)module WordFrequency where

import qualified Data.Map as M
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = M.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr increment M.empty . words
  where
    increment word = M.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topFrequencies :: Int -> FrequencyMap -> [(String, Int)]
topFrequencies n = take n . sortOn (Down . snd) . M.toList

analyzeText :: Int -> String -> [(String, Int)]
analyzeText n = topFrequencies n . countWords

displayAnalysis :: [(String, Int)] -> String
displayAnalysis = unlines . map (\(w, c) -> w ++ ": " ++ show c)