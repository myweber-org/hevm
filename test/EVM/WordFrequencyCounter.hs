module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWordFrequencies :: String -> WordFreq
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalize $ splitText text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

splitText :: String -> [String]
splitText = words . map (\c -> if Char.isAlphaNum c then c else ' ')

normalize :: String -> String
normalize = map Char.toLower . filter Char.isAlpha

getTopNWords :: Int -> WordFreq -> [(String, Int)]
getTopNWords n freqMap =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $ Map.toList freqMap

processText :: String -> Int -> [(String, Int)]
processText text n = getTopNWords n $ countWordFrequencies text

displayResults :: [(String, Int)] -> String
displayResults results =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) resultsmodule WordFrequencyCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countWord [] wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(word, count) -> word ++ ": " ++ show count) counts)

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    text <- getContents
    putStrLn $ processText textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text =
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl updateCount [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    updateCount [] word = [(word, 1)]
    updateCount ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : updateCount rest word

formatResults :: [WordCount] -> String
formatResults counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText content