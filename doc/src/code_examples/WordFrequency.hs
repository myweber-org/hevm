module WordFrequency where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
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
    [] -> interact processText
    [fileName] -> readFile fileName >>= putStrLn . processText
    _ -> putStrLn "Usage: WordFrequency [filename] (reads from stdin if no file)"module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords n