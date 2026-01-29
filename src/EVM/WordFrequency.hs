module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        wordMap = foldr (\word -> insertWith (+) word 1) [] cleanedWords
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    insertWith :: (Int -> Int -> Int) -> String -> Int -> [WordCount] -> [WordCount]
    insertWith f key value [] = [(key, value)]
    insertWith f key value ((k,v):xs)
        | key == k  = (k, f v value) : xs
        | otherwise = (k,v) : insertWith f key value xs

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) words'
        freqMap = foldr (\w m -> insertWord w m) [] cleaned
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

analyzeText :: String -> IO ()
analyzeText input = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ countWords inputmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = map toLower . filter isAlpha
        freqMap = foldr (\w m -> insertWord w m) [] wordsList
        insertWord w [] = [(w, 1)]
        insertWord w ((x, c):xs)
            | w == x = (x, c + 1) : xs
            | otherwise = (x, c) : insertWord w xs
    in sortOn (Down . snd) freqMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> do
            content <- readFile filename
            let frequencies = countWords content
            putStrLn $ formatOutput $ take 10 frequencies