module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        frequencyMap = foldr countWord [] cleanedWords
    in sortOn (Down . snd) frequencyMap
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: String -> [WordCount] -> [WordCount]
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText contentmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

processText :: String -> Int -> [(String, Int)]
processText text n = getTopWords n (countWords text)

main :: IO ()
main = do
    let sampleText = "Hello world hello Haskell World haskell Hello functional world"
    let topWords = processText sampleText 3
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords