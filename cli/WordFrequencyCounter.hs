module WordFrequencyCounter where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = Map.Map String Int

countWordFrequencies :: String -> WordFreq
countWordFrequencies text =
    let words' = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty words'
  where
    cleanWord = map toLower . filter isAlpha

getTopWords :: Int -> WordFreq -> [(String, Int)]
getTopWords n freqMap =
    take n $ sortOn (Down . snd) $ Map.toList freqMap

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

analyzeText :: String -> Int -> String
analyzeText text n =
    let freqMap = countWordFrequencies text
        topWords = getTopWords n freqMap
        totalWords = Map.size freqMap
        totalOccurrences = sum $ Map.elems freqMap
    in "Total unique words: " ++ show totalWords ++ "\n" ++
       "Total word occurrences: " ++ show totalOccurrences ++ "\n" ++
       "Top " ++ show n ++ " words:\n" ++
       displayFrequencies topWordsmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . extractWords
  where
    extractWords = words . map normalizeChar
    normalizeChar c
      | Char.isAlpha c = Char.toLower c
      | otherwise = ' '
    
    incrementWord word = Map.insertWith (+) word 1

topNWords :: Int -> String -> [(String, Int)]
topNWords n = take n . sortByFrequency . Map.toList . countWords
  where
    sortByFrequency = List.sortBy (\(_, c1) (_, c2) -> compare c2 c1)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = group $ sort cleaned
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
    where
        cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello back."
    putStrLn "Word Frequency Report:"
    putStrLn $ wordFrequencyReport sampleText 1 5