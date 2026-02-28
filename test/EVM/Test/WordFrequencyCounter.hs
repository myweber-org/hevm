module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

-- | Count word frequencies in a text string
countWords :: String -> WordCount
countWords text = Map.fromListWith (+) [(w, 1) | w <- words (normalizeText text)]

-- | Normalize text by converting to lowercase and removing non-alphabetic characters
normalizeText :: String -> String
normalizeText = unwords . map (filter isAlpha . map toLower) . words

-- | Get top N most frequent words
topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (\(_, count) -> negate count) . Map.toList

-- | Pretty print word frequencies
printWordFrequencies :: [(String, Int)] -> String
printWordFrequencies freqs = unlines [word ++ ": " ++ show count | (word, count) <- freqs]

-- | Process a text and return top N words
analyzeText :: Int -> String -> String
analyzeText n text = printWordFrequencies $ topNWords n (countWords text)