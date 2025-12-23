module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha . map toLower
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

displayResults :: [WordCount] -> String
displayResults counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText text = 
    let topWords = topNWords 10 text
    in "Top 10 most frequent words:\n" ++ displayResults topWords