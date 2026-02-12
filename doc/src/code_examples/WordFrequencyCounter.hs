module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
  let wordsList = words text
      lowerWords = map (map toLower) wordsList
      sortedWords = sort lowerWords
      grouped = group sortedWords
      frequencies = map (\ws -> (head ws, length ws)) grouped
      sortedFreq = sortOn (Down . snd) frequencies
  in sortedFreq

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqList =
  unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: String -> String
processText = displayFrequencies . countWordFrequencies

main :: IO ()
main = do
  putStrLn "Enter text to analyze word frequencies:"
  input <- getLine
  putStrLn "\nWord frequencies (case-insensitive, sorted by frequency):"
  putStrLn $ processText input