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
    normalize = map Char.toLower . filter Char.isAlphaNum

formatResults :: WordCount -> String
formatResults = unlines . map formatEntry . List.sortOn snd . Map.toList
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: WordFrequencyCounter <text>"
    text -> putStrLn $ processText $ unwords textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in map (\ws -> (head ws, length ws)) grouped

sortByFrequency :: [WordCount] -> [WordCount]
sortByFrequency = sortBy (flip $ comparing snd)

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

processText :: String -> String
processText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getLine
    putStrLn "\nWord frequencies:"
    putStrLn $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text = 
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = 
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs = 
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords n

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell! Haskell is great. World says hello back."
    putStrLn "Top 5 most frequent words:"
    putStrLn $ processText 5 sampleText