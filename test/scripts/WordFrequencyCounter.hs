module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFreq = Map String Int

normalizeWord :: String -> String
normalizeWord = map toLower . filter isAlpha

countWords :: String -> WordFreq
countWords text = foldr incrementWord Map.empty words
  where
    words' = words text
    words = map normalizeWord words'
    incrementWord word freqMap = Map.insertWith (+) word 1 freqMap

getSortedFrequencies :: WordFreq -> [(String, Int)]
getSortedFrequencies = sortOn (negate . snd) . Map.toList

processText :: String -> [(String, Int)]
processText = getSortedFrequencies . countWords

displayResults :: [(String, Int)] -> String
displayResults freqs = unlines $ map formatEntry freqs
  where
    formatEntry (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayResults . processText