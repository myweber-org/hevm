module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord $ topWords 10 text
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show count