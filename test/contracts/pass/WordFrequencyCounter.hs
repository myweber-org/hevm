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
    putStrLn $ "\nTotal unique words: " ++ show (Map.size frequencies)