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
analyzeText = displayResults . countWords