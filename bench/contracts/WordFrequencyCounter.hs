
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
    in map (\ws -> (head ws, length ws)) grouped

cleanWord :: String -> String
cleanWord = filter isAlpha

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortOn (Down . snd)

sortAlphabetically :: [WordCount] -> [WordCount]
sortAlphabetically = sortOn fst

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordCount]
analyzeText text minFreq topN = 
    let counted = countWords text
        filtered = filterByMinFrequency minFreq counted
    in getTopNWords topN filtered

displayResults :: [WordCount] -> IO ()
displayResults counts = 
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) counts

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello to Haskell."
    putStrLn "Word frequency analysis:"
    displayResults $ analyzeText sampleText 1 5module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        grouped = foldr countHelper [] cleaned
        sorted = sortOn (Down . snd) grouped
    in sorted
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
    input <- getContents
    putStrLn $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type WordCount = Map.Map T.Text Int

countWords :: T.Text -> WordCount
countWords text =
    let wordsList = T.words $ T.toLower $ T.filter (\c -> Char.isAlpha c || Char.isSpace c) text
    in Map.fromListWith (+) [(w, 1) | w <- wordsList]

readAndCount :: FilePath -> IO WordCount
readAndCount filePath = do
    content <- TIO.readFile filePath
    return $ countWords content

printTopWords :: Int -> WordCount -> IO ()
printTopWords n wordMap = do
    let sorted = List.sortOn (\(_, count) -> negate count) $ Map.toList wordMap
        topN = take n sorted
    mapM_ (\(word, count) -> TIO.putStrLn $ T.pack (show count) <> " " <> word) topN

processFile :: FilePath -> Int -> IO ()
processFile filePath n = do
    counts <- readAndCount filePath
    putStrLn $ "Top " ++ show n ++ " words in " ++ filePath ++ ":"
    printTopWords n counts

main :: IO ()
main = do
    putStrLn "Enter file path:"
    filePath <- getLine
    putStrLn "Enter number of top words to display:"
    nStr <- getLine
    let n = read nStr :: Int
    processFile filePath n