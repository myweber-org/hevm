module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWordFrequencies :: String -> WordFreq
countWordFrequencies text =
    let wordsList = filter (not . null) $ map normalize $ splitText text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList

splitText :: String -> [String]
splitText = words . map (\c -> if Char.isAlphaNum c then c else ' ')

normalize :: String -> String
normalize = map Char.toLower . filter Char.isAlpha

getTopNWords :: Int -> WordFreq -> [(String, Int)]
getTopNWords n freqMap =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $ Map.toList freqMap

processText :: String -> Int -> [(String, Int)]
processText text n = getTopNWords n $ countWordFrequencies text

displayResults :: [(String, Int)] -> String
displayResults results =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) results