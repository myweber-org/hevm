module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortBy)
import Data.Ord (comparing)

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
        frequencyMap = foldr (\word acc -> case lookup word acc of
                                            Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                                            Nothing -> (word, 1) : acc) [] wordsList
    in sortBy (flip $ comparing snd) frequencyMap

displayTopWords :: Int -> WordCount -> IO ()
displayTopWords n counts = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) $ take n counts

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 10 most frequent words:"
    displayTopWords 10 $ countWords textmodule WordFrequency where

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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

analyzeText :: String -> IO ()
analyzeText text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topNWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show count