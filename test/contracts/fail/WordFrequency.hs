module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

countWords :: String -> Histogram
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: Histogram -> IO ()
printHistogram hist = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') hist
    putStrLn "=========================="

analyzeText :: String -> IO ()
analyzeText text = do
    let freq = countWords text
    putStrLn $ "Total unique words: " ++ show (length freq)
    putStrLn $ "Most frequent word: " ++ fst (head freq)
    printHistogram freqmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map (map toLower) wordsList
        grouped = group $ sort cleaned
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in take 10 $ sortOn (Down . snd) frequencies
  where
    clean = filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram freq = do
    putStrLn "Top 10 most frequent words:"
    putStrLn "----------------------------"
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") freq

analyzeText :: String -> IO ()
analyzeText text = do
    let freq = wordFrequency text
    printHistogram freq

main :: IO ()
main = do
    let sampleText = "This is a sample text. This text contains words. Some words repeat. This is intentional."
    analyzeText sampleText