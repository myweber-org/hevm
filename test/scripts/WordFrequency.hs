module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)

countWords :: String -> Map.Map String Int
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
    in Map.fromListWith (+) [(word, 1) | word <- wordsList]

mostFrequent :: Map.Map String Int -> [(String, Int)]
mostFrequent wordMap = 
    take 5 $ reverse $ sortOn snd $ Map.toList wordMap

displayResults :: String -> IO ()
displayResults text = do
    let freqMap = countWords text
        topWords = mostFrequent freqMap
    
    putStrLn "Top 5 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords
    
    putStrLn $ "\nTotal unique words: " ++ show (Map.size freqMap)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

displayFrequency :: [WordCount] -> String
displayFrequency counts = 
    unlines $ "Top 10 most frequent words:" : map formatCount counts
  where
    formatCount (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = displayFrequency . countWords