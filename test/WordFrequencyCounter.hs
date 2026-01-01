module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: String -> IO ()
displayFrequencies text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ topNWords 10 textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map as Map

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
    in sortOn (negate . snd) $ Map.toList frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: String -> String
processText = displayFrequencies . countWordFrequencies
module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isLetter c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList, not (T.null w)]

sortByFrequency :: WordCount -> [(T.Text, Int)]
sortByFrequency = List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) . Map.toList

filterByMinimumFrequency :: Int -> WordCount -> WordCount
filterByMinimumFrequency minFreq = Map.filter (>= minFreq)

getTopNWords :: Int -> WordCount -> [(T.Text, Int)]
getTopNWords n = take n . sortByFrequency

printWordFrequencies :: [(T.Text, Int)] -> IO ()
printWordFrequencies frequencies = do
    putStrLn "Word frequencies:"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " - " <> word) frequencies

processText :: T.Text -> IO ()
processText text = do
    let wordCount = countWords text
    let filtered = filterByMinimumFrequency 2 wordCount
    let topWords = getTopNWords 10 filtered
    
    putStrLn "\nTop 10 words (minimum frequency 2):"
    printWordFrequencies topWords
    
    let totalWords = Map.foldl' (+) 0 wordCount
    let uniqueWords = Map.size wordCount
    putStrLn $ "\nStatistics:"
    putStrLn $ "Total words: " ++ show totalWords
    putStrLn $ "Unique words: " ++ show uniqueWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    text <- TIO.getContents
    processText text