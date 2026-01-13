module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts

filterByFrequency :: Int -> [WordCount] -> [WordCount]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

displayWordCounts :: [WordCount] -> String
displayWordCounts counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> Int -> String
processText text minFrequency = 
    let counts = countWords text
        filtered = filterByFrequency minFrequency counts
    in displayWordCounts filteredmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

formatHistogram :: [WordCount] -> String
formatHistogram counts = 
    let maxWordLength = maximum $ map (length . fst) counts
        maxCount = maximum $ map snd counts
        scale = 50
    in unlines $ map (\(word, count) -> 
        padRight maxWordLength word ++ " | " ++ 
        replicate (count * scale `div` maxCount) '█' ++ 
        " (" ++ show count ++ ")") counts
  where
    padRight n s = s ++ replicate (n - length s) ' '

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord Frequency Histogram:"
    putStrLn $ formatHistogram $ take 20 frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = groupCount cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    groupCount :: [String] -> [WordCount]
    groupCount = foldr incrementCount []
    
    incrementCount :: String -> [WordCount] -> [WordCount]
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

processText :: String -> String
processText = displayResults . countWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        normalized = map toLower . filter isAlpha
        freqMap = map (\ws -> (head ws, length ws)) . group . sort $ wordsList
    in sortOn (Down . snd) freqMap

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

displayFrequencies :: [WordFreq] -> String
displayFrequencies freqs = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: String -> Int -> Int -> String
processText text minFreq topN = 
    let freqs = countWords text
        filtered = filterByFrequency minFreq freqs
        topWords = getTopNWords topN filtered
    in displayFrequencies topWords

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world programming haskell functional programming"
    putStrLn $ processText sampleText 1 5