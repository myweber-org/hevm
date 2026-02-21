module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (topWords 10 text)module WordFrequency where

import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        freqMap = foldr (\word acc -> insertWord word acc) [] wordsList
    in take 10 $ sortOn (Down . snd) freqMap
  where
    cleanWord = map toLower . filter isAlphaNum
    
    insertWord :: String -> [WordCount] -> [WordCount]
    insertWord word [] = [(word, 1)]
    insertWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : insertWord word rest

displayResults :: [WordCount] -> String
displayResults counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let topWords = countWords input
    putStrLn "\nTop 10 most frequent words:"
    putStrLn $ displayResults topWords