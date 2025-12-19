module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        frequencies = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha

filterByFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> String
wordFrequencyReport text minFreq topN =
    let freqList = countWords text
        filtered = filterByFrequency minFreq freqList
        topWords = getTopNWords topN filtered
    in unlines $ map (\(word, count) -> word ++ ": " ++ show count) topWords

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World says hello back."
    putStrLn "Word Frequency Analysis:"
    putStrLn $ wordFrequencyReport sampleText 1 5