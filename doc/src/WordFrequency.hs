module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr countHelper [] cleaned
        sorted = sortOn (Down . snd) grouped
    in sorted
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

formatResults :: [WordCount] -> String
formatResults counts =
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText = formatResults . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)

countWords :: String -> Map.Map String Int
countWords text = Map.fromListWith (+) [(word, 1) | word <- words text, not (null word)]
  where
    words = map (map toLower . filter isAlpha) . splitBySpace
    splitBySpace = foldr splitter [[]]
    splitter c acc@(current:rest)
      | c == ' '  = []:acc
      | otherwise = (c:current):rest

mostFrequent :: String -> Maybe (String, Int)
mostFrequent text
  | Map.null freqMap = Nothing
  | otherwise = Just (Map.foldrWithKey maxEntry ("", 0) freqMap)
  where
    freqMap = countWords text
    maxEntry word count (maxWord, maxCount)
      | count > maxCount = (word, count)
      | otherwise = (maxWord, maxCount)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map (map toLower) wordsList
        grouped = groupCount cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    groupCount [] = []
    groupCount (x:xs) = 
        let (matches, rest) = partition (== x) xs
            count = 1 + length matches
        in (x, count) : groupCount rest

    partition _ [] = ([], [])
    partition p (y:ys)
        | p y       = let (as, bs) = partition p ys in (y:as, bs)
        | otherwise = let (as, bs) = partition p ys in (as, y:bs)

printWordCounts :: [WordCount] -> IO ()
printWordCounts counts = do
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) counts

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    let frequencies = countWords sampleText
    printWordCounts frequencies