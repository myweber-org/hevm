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
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter isAlpha

sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordFreq]
analyzeText text minFreq topN = 
    getTopNWords topN 
    . filterByMinFrequency minFreq 
    . countWords 
    $ text

printAnalysis :: [WordFreq] -> IO ()
printAnalysis freqList = do
    putStrLn "Word Frequency Analysis:"
    putStrLn "------------------------"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqList
    putStrLn $ "Total unique words: " ++ show (length freqList)

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World of Haskell."
        analysis = analyzeText sampleText 1 5
    printAnalysis analysis