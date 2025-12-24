module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        grouped = foldr countHelper [] wordsList
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha . map toLower
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

topNWords :: Int -> String -> [WordCount]
topNWords n text = take n $ countWords text

displayResults :: [WordCount] -> String
displayResults counts = unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText text = 
    let topWords = topNWords 10 text
    in "Top 10 most frequent words:\n" ++ displayResults topWordsmodule WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' (\acc w -> Map.insertWith (+) w 1 acc) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

getTopNWords :: Int -> String -> [(String, Int)]
getTopNWords n text =
    take n $ List.sortOn (\(_, freq) -> negate freq) $ Map.toList $ countWords text

printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    let topWords = getTopNWords 10 text
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(word, freq) -> putStrLn $ word ++ ": " ++ show freq) topWords

sampleText :: String
sampleText = "This is a sample text. This text contains some words. Some words are repeated. Repeated words help demonstrate frequency counting."

main :: IO ()
main = printWordFrequencies sampleText