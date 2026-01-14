module WordFrequencyCounter where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlphaNum

printFrequencies :: Map.Map String Int -> IO ()
printFrequencies freqMap = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) 
          (Map.toList freqMap)

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun, isn't it?"
    let frequencies = countWords sampleText
    printFrequencies frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
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
cleanWord = filter (\c -> isAlphaNum c || c == '\'')

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordCount] -> [WordCount]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> Int -> Int -> String
processText text minFreq topN = 
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
        topWords = getTopNWords topN filtered
    in formatOutput topWords