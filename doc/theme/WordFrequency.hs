module WordFrequency where

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
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)

countWords :: String -> [(String, Int)]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        sortedWords = sort wordsList
        grouped = group sortedWords
    in sortBy (flip $ comparing snd) $ map (\ws -> (head ws, length ws)) grouped

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqList = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: String -> String
processText = displayFrequencies . countWords