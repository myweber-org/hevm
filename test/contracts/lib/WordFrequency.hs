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
analyzeText n = topWords n . countWordsmodule WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    cleanWord = map toLower . filter isAlpha

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = 
    take n $ sortOn (Down . snd) $ Map.toList $ countWords text

analyzeText :: String -> IO ()
analyzeText text = do
    let topWords = getTopWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) topWords