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
analyzeText = displayResults . processTextmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldl updateFreq [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = filter isAlpha . map toLower
    updateFreq [] word = [(word, 1)]
    updateFreq ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateFreq rest word

formatResults :: [WordFreq] -> String
formatResults freqs = unlines $ map (\(w, c) -> w ++ ": " ++ show c) freqs

processText :: String -> String
processText = formatResults . countWords