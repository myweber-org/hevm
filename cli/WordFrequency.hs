module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map clean $ words text
        cleaned = filter (all isAlpha) words'
        counts = foldl countWord [] cleaned
    in take 10 $ sortOn (Down . snd) counts
  where
    clean = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: [WordCount] -> String -> [WordCount]
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word

displayResults :: [WordCount] -> String
displayResults counts =
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWordsmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
    in Map.fromListWith (+) [(word, 1) | word <- wordsList]

mostFrequent :: Map.Map String Int -> [(String, Int)]
mostFrequent wordMap = 
    take 5 $ reverse $ sortOn snd $ Map.toList wordMap
  where
    sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

displayResults :: String -> IO ()
displayResults input = do
    let freqMap = countWords input
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) $ mostFrequent freqMap

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    text <- getContents
    displayResults textmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr updateFrequency Map.empty . words
  where
    updateFrequency word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topWords n