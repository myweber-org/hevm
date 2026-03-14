
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequency :: String -> String -> Maybe Int
wordFrequency targetWord text = 
    lookup (map toLower targetWord) $ countWords text

totalUniqueWords :: String -> Int
totalUniqueWords = length . countWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWordFrequencies :: String -> [WordFreq]
countWordFrequencies text =
  let wordsList = filter (not . null) $ map cleanWord $ words text
      cleanedWords = map (map toLower) wordsList
      grouped = group $ sort cleanedWords
      frequencies = map (\ws -> (head ws, length ws)) grouped
  in sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

printFrequencies :: [WordFreq] -> IO ()
printFrequencies freqs =
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqs

processText :: String -> Int -> Int -> IO ()
processText text minFreq topN = do
  let allFreqs = countWordFrequencies text
      filtered = filterByFrequency minFreq allFreqs
      topWords = getTopNWords topN filtered
  
  putStrLn $ "Total unique words: " ++ show (length allFreqs)
  putStrLn $ "Words with frequency >= " ++ show minFreq ++ ": " ++ show (length filtered)
  putStrLn "Top words:"
  printFrequencies topWords

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog barked at the fox."

main :: IO ()
main = do
  putStrLn "Word Frequency Counter"
  putStrLn "======================"
  processText sampleText 2 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countHelper [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c+1):rest
        | otherwise = (w, c):countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    let counts = countWords content
    putStrLn $ formatOutput $ take 10 counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _ -> putStrLn "Usage: wordfreq <filename>"module WordFrequencyCounter where

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

filterByFrequency :: Int -> [WordCount] -> [WordCount]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let wordCounts = countWords sampleText
    putStrLn "All word frequencies:"
    printWordCounts wordCounts
    
    putStrLn "\nWords appearing at least 2 times:"
    let filtered = filterByFrequency 2 wordCounts
    printWordCounts filtered