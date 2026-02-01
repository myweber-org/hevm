module WordCounter where

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
    putStrLn $ "Word count: " ++ show wordCount
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
    isPunctuation c = c `elem` ".,!?;:\"\'()[]{}"

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . sortOn (Down . snd) . Map.toList

wordFrequencyReport :: String -> Int -> String
wordFrequencyReport text n =
  let counts = countWords text
      topWords = topNWords n counts
      totalWords = sum (Map.elems counts)
      uniqueWords = Map.size counts
  in unlines $
       [ "Text Analysis Report"
       , "===================="
       , "Total words: " ++ show totalWords
       , "Unique words: " ++ show uniqueWords
       , ""
       , "Top " ++ show n ++ " most frequent words:"
       ] ++
       map (\(word, count) -> word ++ ": " ++ show count) topWords

-- Example usage function
analyzeText :: IO ()
analyzeText = do
  putStrLn "Enter text to analyze (end with empty line):"
  content <- getContents
  let report = wordFrequencyReport content 10
  putStrLn report