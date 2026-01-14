import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: WordFrequencyCounter <text>"
        (text:_) -> do
            let frequencies = countWordFrequencies text
            mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies

countWordFrequencies :: String -> [(String, Int)]
countWordFrequencies text =
    let wordsList = extractWords text
        grouped = groupWords wordsList
        sorted = sortOn (Down . snd) grouped
    in take 10 sorted

extractWords :: String -> [String]
extractWords = words . map normalizeChar
  where
    normalizeChar c
        | isAlpha c = toLower c
        | otherwise = ' '

groupWords :: [String] -> [(String, Int)]
groupWords = foldr incrementCount []
  where
    incrementCount word [] = [(word, 1)]
    incrementCount word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : incrementCount word restmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        wordMap = foldl (\acc word -> case lookup word acc of
                                        Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                                        Nothing -> (word, 1) : acc) [] wordsList
    in sortOn (Down . snd) wordMap

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency (press Ctrl+D when finished):"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by most common):"
    putStr $ formatOutput frequenciesmodule WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

countWords :: String -> Map String Int
countWords = Map.fromListWith (+) . map (\w -> (w, 1)) . words . map toLower

sortByFrequency :: Map String Int -> [(String, Int)]
sortByFrequency = sortOn (\(_, count) -> negate count) . Map.toList

main :: IO ()
main = do
    input <- getContents
    let frequencies = countWords input
    let sorted = sortByFrequency frequencies
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) sorted