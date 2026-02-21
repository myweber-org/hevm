module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = words text
        lowerWords = map (map toLower) words'
        sortedWords = sort lowerWords
        grouped = group sortedWords
        counts = map (\ws -> (head ws, length ws)) grouped
        sortedCounts = sortOn (Down . snd) counts
    in sortedCounts

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    let allCounts = countWords text
        filtered = filterByMinFrequency minFreq allCounts
    in getTopNWords topN filtered

main :: IO ()
main = do
    let sampleText = "Hello world hello Haskell world of functional programming"
    let result = analyzeText sampleText 1 5
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) result