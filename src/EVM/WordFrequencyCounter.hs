module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
        freqs = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) freqs

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = 
    let maxFreq = maximum (map snd freqs)
        scale = 50
        scaleFactor = if maxFreq == 0 then 0 else scale `div` maxFreq
    in mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate (count * scaleFactor) '█' ++ " " ++ show count) freqs

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "========================="
    let freqs = countWords text
    printHistogram $ take 20 freqs
    putStrLn $ "\nTotal unique words: " ++ show (length freqs)
    putStrLn $ "Most frequent word: " ++ fst (head freqs) ++ " (" ++ show (snd (head freqs)) ++ " occurrences)"module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter isAlpha

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordCount] -> [WordCount]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

processText :: String -> Int -> Int -> [WordCount]
processText text minFreq topN = 
    let counted = countWords text
        filtered = filterByMinFrequency minFreq counted
    in getTopNWords topN filtered

displayResults :: [WordCount] -> IO ()
displayResults counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    putStrLn "Word frequency analysis:"
    displayResults $ processText sampleText 1 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr countWord [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

processText :: String -> String
processText = formatResults . countWords