module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha . map toLower
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

displayResults :: [WordCount] -> String
displayResults counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText text = 
    let topWords = topNWords 10 text
    in "Top 10 most frequent words:\n" ++ displayResults topWordsmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' (\acc w -> Map.insertWith (+) w 1 acc) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

getTopNWords :: Int -> String -> [(String, Int)]
getTopNWords n text =
    take n $ List.sortOn (\(_, freq) -> negate freq) $ Map.toList $ countWords text

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let topWords = getTopNWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) topWords

sampleText :: String
sampleText = "This is a sample text. This text contains some words. Some words are repeated. Repeated words help demonstrate frequency counting."

main :: IO ()
main = printWordFrequencies sampleTextmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter isAlpha

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordCount] -> [WordCount]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN =
    let counted = countWords text
        filtered = filterByMinFrequency minFreq counted
    in getTopNWords topN filtered

displayResults :: [WordCount] -> IO ()
displayResults counts = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts
    putStrLn $ "Total unique words: " ++ show (length counts)
    putStrLn $ "Total occurrences: " ++ show (sum $ map snd counts)

sampleAnalysis :: IO ()
sampleAnalysis = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World is imperative."
    let results = analyzeText sampleText 1 5
    displayResults resultsmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter Char.isAlpha . map Char.toLower

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn (negate . snd) . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processFile :: FilePath -> IO ()
processFile path = do
    content <- readFile path
    putStrLn $ formatResults $ countWords content

processStdin :: IO ()
processStdin = do
    content <- getContents
    putStrLn $ formatResults $ countWords content

main :: IO ()
main = do
    args <- Env.getArgs
    case args of
        [] -> processStdin
        [file] -> processFile file
        _ -> putStrLn "Usage: wordfreq [filename] (reads from stdin if no file given)"