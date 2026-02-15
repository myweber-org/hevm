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
            putStrLn $ formatOutput $ take 10 frequenciesmodule WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isAlpha c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]

getTopWords :: Int -> WordCount -> [(T.Text, Int)]
getTopWords n = take n . List.sortOn (\(_, count) -> negate count) . Map.toList

processFile :: FilePath -> Int -> IO ()
processFile filePath n = do
    content <- TIO.readFile filePath
    let frequencies = countWords content
        topWords = getTopWords n frequencies
    
    putStrLn $ "Top " ++ show n ++ " words in " ++ filePath ++ ":"
    mapM_ (\(word, count) -> TIO.putStrLn $ T.concat [word, T.pack ": ", T.pack (show count)]) topWords

main :: IO ()
main = do
    putStrLn "Enter file path:"
    filePath <- getLine
    putStrLn "Enter number of top words to display:"
    nStr <- getLine
    let n = read nStr :: Int
    processFile filePath nmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text = foldr incrementWord Map.empty words
  where
    words' = map normalize (words text)
    words = filter (not . null) words'
    
    normalize = map toLower . filter isAlpha
    
    incrementWord word acc = Map.insertWith (+) word 1 acc

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n sortedFreq
  where
    freqMap = countWords text
    sortedFreq = sortOn (Down . snd) (Map.toList freqMap)

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 10 most frequent words:"
    mapM_ printWord (getTopWords 10 text)
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)
  where
    freqMap = countWords text
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topWords n