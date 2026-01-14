
module WordCounter where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter (not . isPunctuation)
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    isPunctuation c = c `elem` ".,!?;:\"'()[]{}"

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

wordStats :: String -> IO ()
wordStats text = do
  let counts = countWords text
      total = sum counts
      unique = Map.size counts
      top10 = topNWords 10 counts
  
  putStrLn $ "Total words: " ++ show total
  putStrLn $ "Unique words: " ++ show unique
  putStrLn "\nTop 10 words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) top10

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  wordStats content

main :: IO ()
main = do
  putStrLn "Enter text (empty line to finish):"
  input <- getContents
  wordStats inputmodule WordCounter where

import Data.Char (isSpace)
import System.IO (hFlush, stdout)

countWords :: String -> Int
countWords = length . words

main :: IO ()
main = do
    putStr "Enter text: "
    hFlush stdout
    input <- getLine
    let wordCount = countWords input
    putStrLn $ "Word count: " ++ show wordCountmodule WordCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        grouped = group $ sort wordsList
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) frequencies
  where
    normalize = map toLower . filter isAlpha

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minFreq topN =
    let freqs = countWords text
        filtered = filterByMinFrequency minFreq freqs
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello."
    putStrLn "Word Frequency Analysis:"
    putStrLn $ wordFrequencyReport sampleText 1 5