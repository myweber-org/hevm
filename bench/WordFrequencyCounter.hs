module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr incrementWord [] wordsList
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha
    incrementWord word [] = [(word, 1)]
    incrementWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatEntry counts
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
    where
        cleanWord = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = 
    take n $ sortOn (\(_, count) -> negate count) $ Map.toList $ countWords text

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let frequencies = topNWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequenciesmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords n

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello."
    putStrLn "Top 3 most frequent words:"
    putStrLn $ processText 3 sampleTextmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        grouped = group sortedWords
        counts = map (\ws -> (head ws, length ws)) grouped
        filtered = filter (\(_, count) -> count > 1) counts
        sortedCounts = sortOn (Down . snd) filtered
    in sortedCounts

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world test test test"
    let frequencies = countWords sampleText
    printWordFrequencies frequenciesimport Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

countWords :: String -> [(String, Int)]
countWords text = sortOn (Down . snd) $ map (\xs@(x:_) -> (x, length xs)) grouped
  where
    cleaned = map toLower $ filter (\c -> isAlpha c || c == ' ') text
    wordsList = filter (not . null) $ words cleaned
    grouped = group $ sort wordsList

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    input -> do
      let frequencies = countWords $ unwords input
      mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies