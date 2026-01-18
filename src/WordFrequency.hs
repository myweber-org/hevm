module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr countWord [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord w [] = [(w, 1)]
    countWord w ((x, n):xs)
        | w == x = (x, n+1) : xs
        | otherwise = (x, n) : countWord w xs

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatOutput frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        frequencyMap = foldr countWord [] wordsList
    in take 10 $ sortOn (Down . snd) frequencyMap
  where
    cleanWord = map toLower . filter isAlphaNum
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayCounts :: [WordCount] -> String
displayCounts counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again, world!"
    putStrLn "Top 10 most frequent words:"
    putStrLn $ displayCounts $ countWords sampleText