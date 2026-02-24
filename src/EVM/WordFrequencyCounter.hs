module WordFrequencyCounter where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords text =
    let wordsList = filter (not . null) $ map normalize $ splitWords text
    in foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
  where
    splitWords = words . map (\c -> if Char.isAlphaNum c then c else ' ')
    normalize = map Char.toLower

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, cnt1) (_, cnt2) -> compare cnt2 cnt1) $
    Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqs =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqs

processText :: Int -> String -> String
processText n = displayFrequencies . topNWords n