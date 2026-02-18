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
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map toLower <$> wordsList
        wordMap = foldr (\word acc -> 
            case lookup word acc of
                Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                Nothing -> (word, 1) : acc
            ) [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
        uniqueWords = length counts
    in unlines $
        [ "Word Frequency Analysis"
        , "======================"
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 10 words:"
        ] ++
        map (\(word, count) -> word ++ ": " ++ show count) (topNWords 10 text)module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    getTopNWords topN 
    . filterByMinFrequency minFreq 
    . countWords 
    $ text

printWordCounts :: [WordCount] -> IO ()
printWordCounts = mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count)

exampleUsage :: IO ()
exampleUsage = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    let results = analyzeText sampleText 1 5
    putStrLn "Top 5 most frequent words (minimum frequency: 1):"
    printWordCounts results