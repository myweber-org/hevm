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
processText n = displayFrequency . topNWords n