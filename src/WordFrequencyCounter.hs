module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (Down . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map (\(word, count) -> word ++ ": " ++ show count)

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        grouped = foldr countHelper [] cleaned
        sorted = sortOn (Down . snd) grouped
    in sorted
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies:"
    putStrLn $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByFrequency :: Int -> [WordCount] -> [WordCount]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text minFreq topN = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "========================="
    let allCounts = countWords text
        filtered = filterByFrequency minFreq allCounts
        topWords = getTopNWords topN filtered
    printWordCounts topWords
    putStrLn $ "Total unique words: " ++ show (length allCounts)
    putStrLn $ "Words with frequency >= " ++ show minFreq ++ ": " ++ show (length filtered)

sampleAnalysis :: IO ()
sampleAnalysis = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World says hello to Haskell."
    analyzeText sampleText 2 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

cleanWord :: String -> String
cleanWord = map toLower . filter isAlpha

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words text
        cleanedWords = map cleanWord wordsList
        nonEmptyWords = filter (not . null) cleanedWords
        grouped = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] nonEmptyWords
    in sortOn (Down . snd) grouped

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
        uniqueWords = length counts
    in unlines $
        [ "Word Frequency Analysis Report"
        , "============================="
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 10 most frequent words:"
        , "---------------------------"
        ] ++ map (\(w, c) -> w ++ ": " ++ show c) (topNWords 10 text)