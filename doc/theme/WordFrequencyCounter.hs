module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFrequency = Map.Map String Int

countWords :: String -> WordFrequency
countWords text =
    let wordsList = filter (not . null) $ map normalize $ words text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    normalize = map Char.toLower . filter Char.isAlphaNum

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

processText :: Int -> String -> String
processText n = displayFrequency . topNWords nmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = filter (all isAlpha) wordsList
        grouped = group $ sort cleanedWords
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

filterByMinFrequency :: Int -> [WordCount] -> [WordCount]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordCount] -> [WordCount]
getTopNWords n = take n

wordFrequencyReport :: String -> Int -> Int -> [WordCount]
wordFrequencyReport text minFreq topN =
    let counts = countWords text
        filtered = filterByMinFrequency minFreq counts
    in getTopNWords topN filtered

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is great. World says hello back."
    let report = wordFrequencyReport sampleText 1 5
    
    putStrLn "Word Frequency Report:"
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) report