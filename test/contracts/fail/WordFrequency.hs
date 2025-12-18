module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) wordsList
    in sortOn (Down . snd) $ foldl countWord [] cleaned
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord :: [WordCount] -> String -> [WordCount]
    countWord [] word = [(word, 1)]
    countWord ((w, c):rest) word
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord rest word

displayTopWords :: Int -> String -> String
displayTopWords n text = 
    let counts = take n $ countWords text
    in unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts