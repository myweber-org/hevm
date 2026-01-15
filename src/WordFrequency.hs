module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)

countWords :: String -> Map.Map String Int
countWords text = Map.fromListWith (+) wordCounts
  where
    words' = filter (not . null) $ map cleanWord $ words text
    cleanWord = map toLower . filter isAlpha
    wordCounts = zip words' (repeat 1)

displayFrequency :: Map.Map String Int -> String
displayFrequency freqMap = unlines formatted
  where
    sorted = Map.toDescList freqMap
    formatted = map (\(word, count) -> word ++ ": " ++ show count) sorted

analyzeText :: String -> String
analyzeText text = displayFrequency $ countWords text