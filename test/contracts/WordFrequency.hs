module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Top 10 most frequent words:"
  mapM_ printWord (topWords 10 text)
  where
    printWord (word, count) = putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type Frequency = [(String, Int)]

countWords :: String -> Frequency
countWords text =
  let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
      frequencyMap = foldr (\word acc -> case lookup word acc of
                                          Just count -> (word, count + 1) : filter ((/= word) . fst) acc
                                          Nothing    -> (word, 1) : acc) [] wordsList
  in sortOn (Down . snd) frequencyMap

mostFrequent :: Int -> String -> Frequency
mostFrequent n = take n . countWords

displayFrequency :: Frequency -> String
displayFrequency freq = unlines $ map (\(word, count) -> word ++ ": " ++ show count) freq