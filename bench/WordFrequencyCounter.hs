module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        wordMap = foldr incrementWord [] wordsList
    in take 10 $ sortOn (Down . snd) wordMap
  where
    cleanWord = map toLower . filter isAlpha
    incrementWord word [] = [(word, 1)]
    incrementWord word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : incrementWord word rest

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ "Top 10 most frequent words:" : map formatEntry counts
  where
    formatEntry (word, count) = word ++ ": " ++ show count

processText :: String -> String
processText = formatResults . countWordsmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
    where
        cleanWord = filter isAlpha . map toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = 
    take n $ sortOn (\(_, count) -> negate count) $ Map.toList $ countWords text

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let frequencies = topNWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) frequencies