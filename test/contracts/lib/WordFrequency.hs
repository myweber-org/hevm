module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList

analyzeText :: Int -> String -> [(String, Int)]
analyzeText n = topWords n . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlpha

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = 
    take n $ sortOn (Down . snd) $ Map.toList $ countWords text

analyzeText :: String -> IO ()
analyzeText text = do
    let topWords = getTopWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr updateWord Map.empty . words
  where
    updateWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w,c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text =
  let wordsList = filter (not . null) $ map clean $ words text
      cleaned = map (map toLower) wordsList
      grouped = group $ sort cleaned
      counts = map (\ws -> (head ws, length ws)) grouped
  in sortOn (Down . snd) counts
  where
    clean = filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram hist = do
  putStrLn "Word Frequency Histogram:"
  putStrLn "=========================="
  mapM_ (\(word, count) -> 
    putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") hist
  putStrLn "=========================="

main :: IO ()
main = do
  let sampleText = "Hello world hello haskell world programming haskell hello"
  let freq = wordFrequency sampleText
  printHistogram freq