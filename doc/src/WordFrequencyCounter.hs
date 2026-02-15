module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

processFile :: FilePath -> IO ()
processFile filepath = do
  content <- readFile filepath
  let topWords = getTopWords 10 content
  putStrLn "Top 10 most frequent words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWordsmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

prettyPrint :: [(String, Int)] -> String
prettyPrint counts = unlines $ header : map formatRow counts
  where
    header = "Word                | Frequency"
    separator = replicate 22 '-' ++ "+" ++ replicate 11 '-'
    formatRow (word, freq) = printf "%-20s | %10d" word freq

analyzeText :: String -> String
analyzeText text = unlines
  [ "Text Analysis Results:"
  , ""
  , prettyPrint sortedCounts
  , printf "Total unique words: %d" (length sortedCounts)
  , printf "Total words: %d" (sum $ map snd sortedCounts)
  ]
  where
    counts = countWords text
    sortedCounts = sortByFrequency counts

-- Example usage
main :: IO ()
main = do
  let sampleText = "Hello world! Hello Haskell. World of functional programming."
  putStrLn $ analyzeText sampleText