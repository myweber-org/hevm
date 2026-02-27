module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList

analyzeText :: Int -> String -> [(String, Int)]
analyzeText n = topWords n . countWords