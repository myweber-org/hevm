module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = Map.Map String Int

countWordFrequencies :: String -> WordFreq
countWordFrequencies text =
    let words' = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty words'
  where
    cleanWord = map toLower . filter isAlpha

getTopWords :: Int -> WordFreq -> [(String, Int)]
getTopWords n freqMap =
    take n $ sortOn (Down . snd) $ Map.toList freqMap

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

analyzeText :: String -> Int -> String
analyzeText text n =
    let freqMap = countWordFrequencies text
        topWords = getTopWords n freqMap
        totalWords = Map.size freqMap
        totalOccurrences = sum $ Map.elems freqMap
    in "Total unique words: " ++ show totalWords ++ "\n" ++
       "Total word occurrences: " ++ show totalOccurrences ++ "\n" ++
       "Top " ++ show n ++ " words:\n" ++
       displayFrequencies topWords