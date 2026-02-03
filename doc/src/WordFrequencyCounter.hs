module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map formatPair
  where
    formatPair (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText inputmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordFreq] -> IO ()
printHistogram freqs = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") 
        freqs
    putStrLn "=========================="

processText :: String -> IO ()
processText text = do
    let frequencies = countWords text
    putStrLn $ "Total unique words: " ++ show (length frequencies)
    putStrLn $ "Total words: " ++ show (sum $ map snd frequencies)
    printHistogram $ take 10 frequencies

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world programming haskell functional programming"
    processText sampleTextmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanWord = filter isAlpha . map toLower
        frequencyMap = foldl updateCount [] wordsList
        updateCount [] word = [(word, 1)]
        updateCount ((w, c):rest) word
            | w == word = (w, c + 1) : rest
            | otherwise = (w, c) : updateCount rest word
    in sortOn (Down . snd) frequencyMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map formatLine counts
  where
    formatLine (word, count) = word ++ ": " ++ show count

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

type WordCount = Map.Map T.Text Int

stemWord :: T.Text -> T.Text
stemWord word = T.takeWhile isAlphaNum $ T.toLower word

countWords :: T.Text -> WordCount
countWords text = Map.fromListWith (+) wordPairs
  where
    tokens = T.words $ T.map (\c -> if isAlphaNum c then toLower c else ' ') text
    wordPairs = [(stemWord w, 1) | w <- tokens, not (T.null w)]

getTopWords :: Int -> WordCount -> [(T.Text, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

formatOutput :: [(T.Text, Int)] -> T.Text
formatOutput = T.unlines . map (\(w, c) -> T.pack (show c) <> " " <> w)

processFile :: FilePath -> Int -> IO ()
processFile filePath topN = do
    content <- TIO.readFile filePath
    let counts = countWords content
        topWords = getTopWords topN counts
    TIO.putStrLn $ formatOutput topWords

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, nStr] -> 
            case reads nStr of
                [(n, "")] | n > 0 -> processFile filePath n
                _ -> putStrLn "Second argument must be positive integer"
        _ -> putStrLn "Usage: WordFrequencyCounter <file> <top-n>"module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countHelper [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ processText inputmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words $ map toLower text
        sortedWords = sort wordsList
        grouped = group sortedWords
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    let counted = countWords text
        filtered = filterByMinFrequency minFreq counted
    in getTopNWords topN filtered

displayResults :: [WordCount] -> IO ()
displayResults counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world programming haskell functional programming"
    putStrLn "Word frequency analysis:"
    displayResults $ analyzeText sampleText 1 5
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordFrequency = (String, Int)

countWords :: String -> [WordFrequency]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map toLower <$> wordsList
        wordCounts = foldr countWord [] cleanedWords
    in sortOn (Down . snd) wordCounts
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

mostFrequentWords :: Int -> String -> [WordFrequency]
mostFrequentWords n text = take n $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let freqs = countWords text
        totalWords = sum $ map snd freqs
        uniqueWords = length freqs
        topWords = take 5 freqs
    in unlines $
        [ "Text Analysis Report"
        , "==================="
        , "Total words: " ++ show totalWords
        , "Unique words: " ++ show uniqueWords
        , ""
        , "Top 5 most frequent words:"
        ] ++ map (\(w, c) -> "  " ++ w ++ ": " ++ show c) topWords