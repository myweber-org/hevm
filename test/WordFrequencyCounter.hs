module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: String -> IO ()
displayFrequencies text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ topNWords 10 textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map as Map

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
    in sortOn (negate . snd) $ Map.toList frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: String -> String
processText = displayFrequencies . countWordFrequencies