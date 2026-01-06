
module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = unlines $
  "Total unique words: " ++ show (Map.size counts) :
  "Top 10 most frequent words:" :
  map formatWord (topNWords 10 text)
  where
    counts = countWords text
    formatWord (word, count) = "  " ++ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayTopWords :: Int -> String -> IO ()
displayTopWords n text = do
  putStrLn $ "Top " ++ show n ++ " words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) (topNWords n text)module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequencies:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatResults frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Set as Set

type Word = String

stem :: Word -> Word
stem w = case reverse w of
    ('s':xs) -> reverse xs
    ('g':'n':xs) -> reverse ('g':xs)
    _ -> w

stopWords :: Set.Set Word
stopWords = Set.fromList ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for", "of", "with", "by"]

cleanWord :: Word -> Maybe Word
cleanWord w = 
    let lower = map toLower w
        filtered = filter isAlpha lower
    in if null filtered || Set.member filtered stopWords
       then Nothing
       else Just (stem filtered)

countWords :: [Word] -> [(Word, Int)]
countWords = map (\ws -> (head ws, length ws)) 
           . group 
           . sort 
           . map (\(Just w) -> w) 
           . filter (/= Nothing) 
           . map cleanWord

topNWords :: Int -> [Word] -> [(Word, Int)]
topNWords n = take n 
            . sortOn (Down . snd) 
            . countWords

processText :: String -> [(Word, Int)]
processText = topNWords 10 . words

main :: IO ()
main = do
    let sample = "The quick brown fox jumps over the lazy dog. The dog barks at the fox, but the fox runs away quickly."
    print $ processText samplemodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords