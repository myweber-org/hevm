module WordCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayResults :: [(String, Int)] -> String
displayResults = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayResults . topWords n