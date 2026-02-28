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
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr countWord [] wordsList
    in sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
        uniqueWords = length counts
    in unlines $
        [ "Word Frequency Analysis"
        , "======================"
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 10 words:"
        ] ++ map (\(w, c) -> w ++ ": " ++ show c) (topNWords 10 text)module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFreq = Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: String -> IO ()
displayFrequencies text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWords
  where topWords = getTopWords 10 text

main :: IO ()
main = do
  let sampleText = "Hello world! This is a test. Hello again, world!"
  displayFrequencies sampleText