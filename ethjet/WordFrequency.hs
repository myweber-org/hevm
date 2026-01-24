module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) counts

analyzeText :: String -> String
analyzeText = displayFrequency . countWordsmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = foldr countWord [] cleanedWords
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = unlines $ map showEntry counts
  where
    showEntry (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText text = 
    let frequencies = countWords text
    in "Top 10 most frequent words:\n" ++ displayFrequency frequenciesmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

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

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

analyzeText :: String -> String
analyzeText = formatResults . countWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    putStrLn "\nWord frequencies:"
    putStrLn $ analyzeText contentmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

countWords :: String -> Histogram
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
        grouped = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts

printHistogram :: Histogram -> IO ()
printHistogram hist = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') hist
    putStrLn "========================="

main :: IO ()
main = do
    let sampleText = "Hello world hello haskell world world functional programming"
    let freq = countWords sampleText
    printHistogram freq