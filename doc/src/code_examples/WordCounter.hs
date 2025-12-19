module WordCounter where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (words)

countWords :: String -> Map.Map String Int
countWords text = 
    let cleanedWords = map (map toLower . filter isAlpha) (words text)
        validWords = filter (not . null) cleanedWords
    in Map.fromListWith (+) [(word, 1) | word <- validWords]

displayWordCount :: Map.Map String Int -> String
displayWordCount wordMap = 
    unlines [word ++ ": " ++ show count | (word, count) <- Map.toList wordMap]

processText :: String -> String
processText text = displayWordCount (countWords text)