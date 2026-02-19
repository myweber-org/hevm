module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByFrequency :: Int -> [WordCount] -> [WordCount]
filterByFrequency minCount = filter (\(_, count) -> count >= minCount)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

processText :: String -> Int -> Int -> IO ()
processText text minFreq topN = do
    putStrLn "Word frequencies:"
    let counts = countWords text
    let filtered = filterByFrequency minFreq counts
    let topWords = getTopNWords topN filtered
    printWordCounts topWords

exampleUsage :: IO ()
exampleUsage = do
    let sampleText = "Hello world! Hello Haskell. Haskell is functional. World of Haskell."
    putStrLn "Processing sample text..."
    processText sampleText 1 10