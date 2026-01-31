module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr insertWord Map.empty . words
  where
    insertWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> String
displayTopWords n text = unlines $ map format $ topNWords n text
  where
    format (word, count) = word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w m -> insertWord w m) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    insertWord w [] = [(w, 1)]
    insertWord w ((x, n):xs)
        | w == x = (x, n+1):xs
        | otherwise = (x, n):insertWord w xs

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text (press Ctrl+D when finished):"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies:"
    putStrLn $ formatOutput frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text =
  let words' = filter (not . null) $ map clean $ words text
      cleaned = map (map toLower) words'
      grouped = group $ sort cleaned
      counts = map (\ws -> (head ws, length ws)) grouped
  in sortOn (Down . snd) counts
  where
    clean = filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram freq = do
  putStrLn "Word Frequency Histogram:"
  putStrLn "=========================="
  mapM_ (\(word, count) -> 
    putStrLn $ word ++ ": " ++ replicate count '*') freq
  putStrLn "=========================="

analyzeText :: String -> IO ()
analyzeText text = do
  let freq = wordFrequency text
  putStrLn $ "Total unique words: " ++ show (length freq)
  putStrLn $ "Most frequent word: " ++ fst (head freq)
  printHistogram $ take 10 freqmodule WordFrequency where

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

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
  putStrLn $ "Top " ++ show n ++ " words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) (topWords n text)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords text = foldr incrementWord Map.empty words
  where
    words' = map (map toLower) $ filter (all isAlpha) $ words text
    incrementWord word acc = Map.insertWith (+) word 1 acc

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (\(_, count) -> -count) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
    putStrLn $ "Top " ++ show n ++ " most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) (topNWords n text)

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing word frequency."
    displayTopWords 5 sampleText