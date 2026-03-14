module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords nmodule WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlpha)

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

mostFrequent :: WordFreq -> [(String, Int)]
mostFrequent = take 5 . sortByFrequency . Map.toList
  where
    sortByFrequency = reverse . sortOn snd

displayFrequency :: String -> IO ()
displayFrequency text = do
  let freq = countWords text
      topWords = mostFrequent freq
  putStrLn "Top 5 most frequent words:"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords