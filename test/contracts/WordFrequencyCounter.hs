import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: WordFrequencyCounter <text>"
        (text:_) -> do
            let frequencies = countWordFrequencies text
            mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = extractWords text
        grouped = groupWords wordsList
        sorted = sortOn (Down . snd) grouped
    in take 10 sorted

extractWords :: String -> [String]
extractWords = words . map normalizeChar
  where
    normalizeChar c
        | isAlpha c = toLower c
        | otherwise = ' '

groupWords :: [String] -> [(String, Int)]
groupWords = foldr incrementCount []
  where
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word restmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        wordMap = foldl (\acc word -> case lookup word acc of
                                        Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                                        Nothing -> (word, 1) : acc) [] wordsList
    in sortOn (Down . snd) wordMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency (press Ctrl+D when finished):"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by most common):"
    putStr $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

countWords :: String -> Map String Int
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . words . map toLower

sortByFrequency :: Map String Int -> [(String, Int)]
sortByFrequency = sortOn (\(_, count) -> negate count) . Map.toList

main :: IO ()
main = do
    input <- getContents
    let frequencies = countWords input
    let sorted = sortByFrequency frequencies
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) sortedmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words $ map normalizeChar text
        normalized = filter (not . null) $ map normalizeWord wordsList
        grouped = group $ sort normalized
    in map (\ws -> (head ws, length ws)) grouped
  where
    normalizeChar c
        | c `elem` ".,!?;:\"()[]{}" = ' '
        | otherwise = c
    normalizeWord = filter (`notElem` "'-") . map toLower

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

displayWordCounts :: [WordCount] -> String
displayWordCounts counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText text =
    let report = wordFrequencyReport text 2 10
    in "Top 10 words appearing at least twice:\n" ++ displayWordCounts report
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
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter (\(_, count) -> count >= minFreq)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

analyzeText :: String -> Int -> Int -> IO ()
analyzeText text minFreq topN = do
    let allCounts = countWords text
    let filtered = filterByMinFrequency minFreq allCounts
    let topWords = getTopNWords topN filtered
    
    putStrLn $ "Total unique words: " ++ show (length allCounts)
    putStrLn $ "Words with frequency >= " ++ show minFreq ++ ": " ++ show (length filtered)
    putStrLn $ "Top " ++ show topN ++ " words:"
    printWordCounts topWords

sampleText :: String
sampleText = "The quick brown fox jumps over the lazy dog. The dog was not amused by the fox's antics. Quick thinking saved the day."

main :: IO ()
main = do
    putStrLn "Word Frequency Analysis"
    putStrLn "======================="
    analyzeText sampleText 2 5module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

countWords :: T.Text -> Map.Map T.Text Int
countWords text = Map.fromListWith (+) [(T.toLower word, 1) | word <- T.words text]

cleanWord :: T.Text -> T.Text
cleanWord = T.filter Char.isAlpha . T.toLower

countWordsCleaned :: T.Text -> Map.Map T.Text Int
countWordsCleaned text = Map.fromListWith (+) [(cleanWord word, 1) | word <- T.words text, not (T.null (cleanWord word))]

sortByFrequency :: Map.Map T.Text Int -> [(T.Text, Int)]
sortByFrequency = List.sortOn (\(_, count) -> -count) . Map.toList

printWordFrequencies :: [(T.Text, Int)] -> IO ()
printWordFrequencies frequencies = mapM_ (\(word, count) -> TIO.putStrLn (T.pack (show count) <> " " <> word)) frequencies

main :: IO ()
main = do
    input <- TIO.getContents
    let frequencies = sortByFrequency (countWordsCleaned input)
    printWordFrequencies frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

displayHistogram :: [WordCount] -> String
displayHistogram counts = 
    let maxCount = maximum $ map snd counts
        scale = 50.0 / fromIntegral maxCount
        bar count = replicate (round $ fromIntegral count * scale) '█'
    in unlines $ map (\(word, count) -> word ++ ": " ++ bar count ++ " (" ++ show count ++ ")") counts

analyzeText :: String -> String
analyzeText text = 
    let counts = countWords text
        totalWords = sum $ map snd counts
        uniqueWords = length counts
    in "Total words: " ++ show totalWords ++ 
       "\nUnique words: " ++ show uniqueWords ++ 
       "\n\nWord frequency histogram:\n" ++ displayHistogram (take 20 counts)