module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = filter Char.isAlpha . map Char.toLower

getTopWords :: Int -> WordCount -> [(String, Int)]
getTopWords n = take n . List.sortBy (\(_, a) (_, b) -> compare b a) . Map.toList

displayResults :: [(String, Int)] -> IO ()
displayResults results = do
    putStrLn "Top words:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) results

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing word frequency."
    let wordCounts = countWords sampleText
    let topWords = getTopWords 5 wordCounts
    displayResults topWords