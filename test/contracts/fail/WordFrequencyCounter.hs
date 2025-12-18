module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
    in sortOn (Down . snd) $ countOccurrences cleanedWords

cleanWord :: String -> String
cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

countOccurrences :: [String] -> [WordCount]
countOccurrences = foldr incrementCount []
  where
    incrementCount word counts =
        case lookup word counts of
            Just n -> (word, n + 1) : filter ((/= word) . fst) counts
            Nothing -> (word, 1) : counts

getTopWords :: Int -> String -> [WordCount]
getTopWords n text = take n $ countWords text

formatResults :: [WordCount] -> String
formatResults counts = unlines $ map formatPair counts
  where
    formatPair (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText text = 
    let totalWords = length $ words text
        uniqueWords = length $ countWords text
        topWords = getTopWords 10 text
    in "Total words: " ++ show totalWords ++ "\n" ++
       "Unique words: " ++ show uniqueWords ++ "\n" ++
       "Top 10 words:\n" ++ formatResults topWords