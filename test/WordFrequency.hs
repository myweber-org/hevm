module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr countHelper [] cleaned
    in take 10 $ sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

displayResults :: [WordCount] -> String
displayResults counts = 
    "Top 10 most frequent words:\n" ++
    unlines (map (\(w, c) -> w ++ ": " ++ show c) counts)

analyzeText :: String -> String
analyzeText = displayResults . countWords