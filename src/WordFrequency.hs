
module WordFrequency where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: [String] -> WordCount
countWords = foldr incrementWord Map.empty . concatMap words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

topWords :: Int -> [String] -> [(String, Int)]
topWords n texts = take n $ sortOn (negate . snd) $ Map.toList $ countWords texts

displayFrequency :: [(String, Int)] -> String
displayFrequency counts = unlines $ map format counts
  where
    format (word, count) = word ++ ": " ++ show count