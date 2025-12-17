module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

getTopWords :: Int -> FrequencyMap -> [(String, Int)]
getTopWords n = take n . sortOn (Down . snd) . Map.toList

analyzeText :: Int -> String -> [(String, Int)]
analyzeText n = getTopWords n . countWords