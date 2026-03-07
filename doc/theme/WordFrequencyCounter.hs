module WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text = 
    let words' = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty words'

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = 
    take n $ sortOn (Down . snd) $ Map.toList freqMap

filterByMinFrequency :: Int -> WordFrequency -> WordFrequency
filterByMinFrequency minFreq = Map.filter (>= minFreq)

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text topN minFreq = do
    let freqMap = countWords text
    let filteredMap = filterByMinFrequency minFreq freqMap
    let topWords = getTopWords topN filteredMap
    
    putStrLn "Top words by frequency:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)
    putStrLn $ "Words meeting minimum frequency (" ++ show minFreq ++ "): " ++ show (Map.size filteredMap)

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis"
    putStrLn "======================="
    analyzeText sampleText 5 2module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . filter (not . T.null) . map normalize . T.words
  where
    normalize = T.toCaseFold . T.filter Char.isLetter

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

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

filterByMinFrequency :: Int -> WordCount -> WordCount
filterByMinFrequency minFreq = Map.filter (>= minFreq)

getTopNWords :: Int -> WordCount -> [(String, Int)]
getTopNWords n = take n . sortOn (Down . snd) . Map.toList

analyzeText :: String -> Int -> Int -> [(String, Int)]
analyzeText text minFreq topN = 
    getTopNWords topN $ filterByMinFrequency minFreq $ countWords text

displayAnalysis :: String -> Int -> Int -> IO ()
displayAnalysis text minFreq topN = do
    let results = analyzeText text minFreq topN
    putStrLn "Top words by frequency:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) results

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog barks at the fox."

main :: IO ()
main = displayAnalysis sampleText 2 5