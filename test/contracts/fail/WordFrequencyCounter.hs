module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

countWordFrequency :: String -> [(String, Int)]
countWordFrequency text =
    let wordsList = words text
        lowerWords = map (map toLower) wordsList
        sortedWords = sort lowerWords
        grouped = group sortedWords
        frequencies = map (\ws -> (head ws, length ws)) grouped
        sortedFreq = sortOn (Down . snd) frequencies
    in sortedFreq

formatOutput :: [(String, Int)] -> String
formatOutput frequencies =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) frequencies

processText :: String -> String
processText = formatOutput . countWordFrequency

main :: IO ()
main = do
    input <- getContents
    putStr $ processText inputmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified System.Environment as Env

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

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> putStrLn "Usage: wordfreq <text>"
    textPieces -> do
      let text = unwords textPieces
      putStrLn "Word frequencies:"
      putStrLn . formatResults $ countWords text