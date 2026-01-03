module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    putStrLn "Word frequencies:"
    let frequencies = countWords sampleText
    printWordCounts frequencies
    
    putStrLn "\nWords with frequency >= 2:"
    let filtered = filterByMinFrequency 2 frequencies
    printWordCounts filtered
    
    putStrLn "\nTop 3 words:"
    let top3 = getTopNWords 3 frequencies
    printWordCounts top3