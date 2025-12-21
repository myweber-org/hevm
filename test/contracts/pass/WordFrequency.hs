module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word acc =
      let cleaned = filter isAlpha (map toLower word)
       in if null cleaned
            then acc
            else Map.insertWith (+) cleaned 1 acc

topWords :: Int -> WordCount -> [(String, Int)]
topWords n = take n . sortOn (negate . snd) . Map.toList

analyzeText :: String -> Int -> [(String, Int)]
analyzeText text n = topWords n (countWords text)

displayResults :: [(String, Int)] -> String
displayResults = unlines . map (\(w, c) -> w ++ ": " ++ show c)