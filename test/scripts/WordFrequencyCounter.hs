module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter Char.isAlpha . map Char.toLower

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    textPieces -> do
      let text = unwords textPieces
      let frequencyMap = countWords text
      putStrLn $ formatResults frequencyMapmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

exampleUsage :: IO ()
exampleUsage = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    let report = wordFrequencyReport sampleText 1 5
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) reportmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        counts = foldl countWord [] cleaned
    in sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getLine
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
    in sortOn (\(_, count) -> -count) $ Map.toList frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of words in a text string
countWordFrequency :: String -> [WordCount]
countWordFrequency text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        frequencyMap = foldr countWord [] wordsList
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    normalize = filter isAlpha . map toLower
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

-- | Pretty print word frequency results
printWordFrequency :: [WordCount] -> IO ()
printWordFrequency counts = do
    putStrLn "Top 10 most frequent words:"
    putStrLn "----------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

-- | Process a sample text and display results
sampleAnalysis :: IO ()
sampleAnalysis = do
    let sampleText = "This is a sample text. This text contains words. Some words repeat. This is intentional."
    let frequencies = countWordFrequency sampleText
    printWordFrequency frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) words'
    in sortOn (Down . snd) $ map (\w -> (w, length $ filter (== w) cleaned)) (nub cleaned)
    where
        cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
    in unlines $
        "Total unique words: " ++ show (length counts) :
        "Total word occurrences: " ++ show totalWords :
        "" :
        "Top 10 most frequent words:" :
        map (\(w, c) -> w ++ ": " ++ show c) (take 10 counts)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)