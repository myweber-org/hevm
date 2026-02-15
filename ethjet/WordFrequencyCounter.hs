module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toLower . T.filter Char.isLetter

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, c1) (_, c2) -> compare c2 c1) . Map.toList

formatOutput :: [(T.Text, Int)] -> T.Text
formatOutput = T.unlines . map (\(word, count) -> T.pack (show count) <> " " <> word)

processText :: T.Text -> T.Text
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- TIO.getContents
    TIO.putStr $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of words in a text string
--   Converts to lowercase and filters non-alphabetic characters
countWordFrequencies :: String -> [WordCount]
countWordFrequencies text =
  let wordsList = words text
      cleanedWords = map (filter isAlpha . map toLower) wordsList
      nonEmptyWords = filter (not . null) cleanedWords
      frequencyMap = foldr countWord [] nonEmptyWords
  in sortOn (Down . snd) frequencyMap
  where
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
      | w == word = (w, c + 1) : rest
      | otherwise = (w, c) : countWord word rest

-- | Get top N most frequent words
topNWords :: Int -> [WordCount] -> [WordCount]
topNWords n = take n

-- | Format word frequencies for display
formatFrequencies :: [WordCount] -> String
formatFrequencies freqs =
  unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

-- | Process a text and return formatted top N words
analyzeText :: Int -> String -> String
analyzeText n text =
  let freqs = countWordFrequencies text
      topWords = topNWords n freqs
  in formatFrequencies topWords