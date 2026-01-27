module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        wordMap = foldl countWord emptyMap cleanedWords
    in take 10 $ sortOn (Down . snd) $ toList wordMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    emptyMap = []
    
    countWord :: [WordCount] -> String -> [WordCount]
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word
    
    toList = id

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let frequencies = countWords text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

processTextFile :: FilePath -> IO ()
processTextFile filePath = do
    content <- readFile filePath
    printWordFrequencies contentmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
    in sortOn (Down . snd) $ foldr countWord [] cleaned
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countWord w [] = [(w, 1)]
    countWord w ((x, n):xs)
        | w == x = (x, n+1):xs
        | otherwise = (x, n):countWord w xs

printWordFrequencies :: [WordCount] -> IO ()
printWordFrequencies counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Word frequencies:"
    printWordFrequencies $ countWords text
    putStrLn $ "\nTotal unique words: " ++ show (length $ countWords text)module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> String
displayTopWords n text = unlines $ map format $ topWords n text
  where
    format (word, count) = word ++ ": " ++ show count

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  putStrLn $ displayTopWords 10 text
  putStrLn $ "Total unique words: " ++ show (Map.size $ countWords text)