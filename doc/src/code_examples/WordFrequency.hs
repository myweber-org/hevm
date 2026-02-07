
import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Environment (getArgs)

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower wordsList
        grouped = foldr (\w m -> insertWith (+) w 1 m) [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    insertWith f key value [] = [(key, value)]
    insertWith f key value ((k,v):rest)
        | key == k  = (k, f v value) : rest
        | otherwise = (k,v) : insertWith f key value rest

formatOutput :: [WordCount] -> String
formatOutput counts = unlines $ map (\(w,c) -> w ++ ": " ++ show c) counts

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: wordfreq <filename>"
        (filename:_) -> do
            content <- readFile filename
            putStrLn $ formatOutput $ countWords contentmodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words . map normalize
  where
    normalize c
      | isAlpha c = toLower c
      | otherwise = ' '
    
    incrementWord word = Map.insertWith (+) word 1

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords n