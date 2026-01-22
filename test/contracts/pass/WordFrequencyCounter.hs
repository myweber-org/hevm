module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in List.foldl' incrementWord Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower
    incrementWord m word = Map.insertWith (+) word 1 m

topNWords :: Int -> WordCount -> [(String, Int)]
topNWords n = take n . List.sortBy compareCount . Map.toList
  where
    compareCount (_, cnt1) (_, cnt2) = compare cnt2 cnt1

displayResults :: [(String, Int)] -> String
displayResults results =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) results

analyzeText :: Int -> String -> String
analyzeText n text =
    let wordCounts = countWords text
        topWords = topNWords n wordCounts
    in displayResults topWords

main :: IO ()
main = do
    let sampleText = "This is a test. This test is only a test. Testing testing one two three."
    putStrLn $ analyzeText 5 sampleText