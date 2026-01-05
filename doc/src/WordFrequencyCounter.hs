module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map formatPair
  where
    formatPair (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") 
        freqs
    putStrLn "=========================="

processText :: String -> IO ()
processText text = do
    let frequencies = countWords text
    putStrLn $ "Total unique words: " ++ show (length frequencies)
    putStrLn $ "Total words: " ++ show (sum $ map snd frequencies)
    printHistogram $ take 10 frequencies

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world programming haskell functional programming"
    processText sampleTextmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = filter isAlpha . map toLower
        frequencyMap = foldl updateCount [] wordsList
        updateCount [] word = [(word, 1)]
        updateCount ((w, c):rest) word
            | w == word = (w, c + 1) : rest
            | otherwise = (w, c) : updateCount rest word
    in sortOn (Down . snd) frequencyMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequencies