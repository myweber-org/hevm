module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        wordMap = foldl (\acc w -> insertWord w acc) [] cleanedWords
    in sortOn (Down . snd) wordMap
  where
    cleanWord = filter (\c -> isAlphaNum c || c == '\'')

insertWord :: String -> [WordCount] -> [WordCount]
insertWord word [] = [(word, 1)]
insertWord word ((w, c):rest)
    | w == word = (w, c + 1) : rest
    | otherwise = (w, c) : insertWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let frequencies = countWords input
    putStrLn "\nWord frequencies (sorted by count):"
    putStrLn $ formatResults frequenciesmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show count