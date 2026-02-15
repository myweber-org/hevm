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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (\(_, count) -> negate count) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(word, count) -> word ++ ": " ++ show count)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topNWords n