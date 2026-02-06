module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' incrementWord Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower
    incrementWord m word = Map.insertWith (+) word 1 m

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . List.sortBy compareCount . Map.toList
  where
    compareCount (_, cnt1) (_, cnt2) = compare cnt2 cnt1

displayResults :: [(String, Int)] -> String
displayResults results =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) results

analyzeText :: Int -> String -> String
analyzeText n text =
    let wordCounts = countWords text
        topWords = topNWords n wordCounts
    in displayResults topWords

main :: IO ()
main = do
    let sampleText = "This is a test. This test is only a test. Testing testing one two three."
    putStrLn $ analyzeText 5 sampleTextmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWordFrequencies :: String -> WordFreq
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalize $ splitWords text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

splitWords :: String -> [String]
splitWords = words . map (\c -> if Char.isAlpha c then c else ' ')

normalize :: String -> String
normalize = map Char.toLower . filter Char.isAlpha

getTopNWords :: Int -> WordFreq -> [(String, Int)]
getTopNWords n freqMap =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $ Map.toList freqMap

printWordFrequencies :: [(String, Int)] -> IO ()
printWordFrequencies freqList =
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqList

processText :: String -> IO ()
processText text = do
    let frequencies = countWordFrequencies text
    let topWords = getTopNWords 10 frequencies
    putStrLn "Top 10 most frequent words:"
    printWordFrequencies topWords
    putStrLn $ "\nTotal unique words: " ++ show (Map.size frequencies)module WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

countWords :: String -> Map.Map String Int
countWords text = Map.fromListWith (+) wordCounts
  where
    words' = filter (not . null) . map (map toLower . filter isAlphaNum) $ splitWords text
    wordCounts = map (\w -> (w, 1)) words'

splitWords :: String -> [String]
splitWords [] = []
splitWords str = word : splitWords rest
  where
    (word, rest') = break (not . isAlphaNum) str
    rest = dropWhile (not . isAlphaNum) rest'

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (\(_, count) -> negate count) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
    putStrLn $ "Top " ++ show n ++ " words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
  where
    topWords = getTopWords n textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

sortWordCounts :: WordCount -> [(String, Int)]
sortWordCounts = sortOn (Down . snd) . Map.toList

formatResults :: [(String, Int)] -> String
formatResults = unlines . map formatRow
  where
    formatRow (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = formatResults . sortWordCounts . countWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."
    putStrLn $ analyzeText sampleText