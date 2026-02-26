
module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = foldr countWord [] cleanedWords
    in sortOn (Down . snd) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')
    
    countWord word [] = [(word, 1)]
    countWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countWord word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

wordFrequency :: String -> String -> Maybe Int
wordFrequency targetWord text = 
    lookup (map toLower targetWord) $ countWords text

totalUniqueWords :: String -> Int
totalUniqueWords = length . countWords