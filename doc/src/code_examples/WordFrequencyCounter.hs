module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter Char.isAlpha . map Char.toLower

formatResults :: WordCount -> String
formatResults = unlines . map formatItem . List.sortOn snd . Map.toList
  where
    formatItem (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please provide text as argument"
    textArgs -> putStrLn $ processText $ unwords textArgsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl (\acc word -> insertWord word acc) [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

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
    putStrLn $ processText input