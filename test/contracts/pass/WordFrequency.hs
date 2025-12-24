
module TextUtils.WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

wordFrequencyReport :: String -> String
wordFrequencyReport text = unlines $
  "Total unique words: " ++ show (Map.size counts) :
  "Top 10 most frequent words:" :
  map formatWord (topNWords 10 text)
  where
    counts = countWords text
    formatWord (word, count) = "  " ++ word ++ ": " ++ show count