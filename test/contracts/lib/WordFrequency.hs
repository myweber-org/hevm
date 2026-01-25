module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl updateCount [] cleanedWords
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    updateCount [] word = [(word, 1)]
    updateCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateCount rest word

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText text = 
    let frequencies = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

processText :: String -> String
processText = displayResults . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        grouped = foldr (\word acc -> insertWord word acc) [] wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    normalize = map toLower . filter isAlphaNum
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

processFile :: FilePath -> IO ()
processFile path = do
    content <- readFile path
    let topWords = countWords content
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

wordFrequency :: String -> [WordFreq]
wordFrequency text =
    let cleaned = map toLower text
        words' = words cleaned
        filtered = filter (all isAlpha) words'
        freqMap = foldr (\w m -> insertWord w m) [] filtered
        sorted = sortOn (Down . snd) freqMap
    in sorted

insertWord :: String -> [WordFreq] -> [WordFreq]
insertWord word [] = [(word, 1)]
insertWord word ((w, c):rest)
    | w == word = (w, c + 1) : rest
    | otherwise = (w, c) : insertWord word rest

analyzeText :: String -> IO ()
analyzeText text = do
    let freqs = wordFrequency text
    putStrLn "Word frequency analysis:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) freqs