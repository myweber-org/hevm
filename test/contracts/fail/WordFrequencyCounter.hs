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
      putStrLn . formatResults $ countWords textmodule WordFrequencyCounter where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr (\w m -> insertWord w m) [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord w [] = [(w, 1)]
    insertWord w ((word, count):rest)
        | w == word = (word, count + 1) : rest
        | otherwise = (word, count) : insertWord w rest

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filename = do
    contents <- readFile filename
    let frequencies = countWords contents
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> processFile filenamemodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFreq = Map String Int

countWords :: String -> WordFreq
countWords = foldr updateFreq Map.empty . words
  where
    updateFreq word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

processText :: Int -> String -> String
processText n = displayFrequencies . getTopWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w acc -> case lookup w acc of
                                    Just count -> (w, count + 1) : filter ((/= w) . fst) acc
                                    Nothing -> (w, 1) : acc) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processFile :: FilePath -> IO ()
processFile filepath = do
    content <- readFile filepath
    let frequencies = countWords content
    putStrLn $ formatOutput frequencies

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> processFile filepath
        _ -> putStrLn "Usage: wordfreq <filename>"