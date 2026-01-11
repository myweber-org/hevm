module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
        grouped = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts

printHistogram :: Histogram -> IO ()
printHistogram freq = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ replicate count '*') freq

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let freq = wordFrequency input
    printHistogram freqmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList . countWords

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: WordCount -> String
displayResults counts = unlines $
    "Top 10 most frequent words:" : 
    map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words . map normalize
  where
    normalize c = if isAlpha c then toLower c else ' '
    incrementWord word = Map.insertWith (+) word 1

topWords :: Int -> String -> [(String, Int)]
topWords n = take n . sortOn (negate . snd) . Map.toList . countWords

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr countWord [] wordsList
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequencies :: [WordCount] -> String
displayFrequencies counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = displayFrequencies . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w acc -> case lookup w acc of
                                    Just count -> (w, count + 1) : filter ((/= w) . fst) acc
                                    Nothing -> (w, 1) : acc) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile path = do
    content <- readFile path
    let frequencies = countWords content
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    putStrLn "Enter file path:"
    filePath <- getLine
    processFile filePath