module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordFreq]
analyzeText text minFreq topN = 
    getTopNWords topN . filterByMinFrequency minFreq . countWords $ text

displayResults :: [WordFreq] -> IO ()
displayResults freqs = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqs
    putStrLn $ "Total unique words: " ++ show (length freqs)

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    let results = analyzeText sampleText 1 5
    displayResults results