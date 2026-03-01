module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr updateCount Map.empty . words
  where
    updateCount word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

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
        cleaned = map toLower <$> wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayFrequency . countWordsmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' (\acc w -> Map.insertWith (+) w 1 acc) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords n