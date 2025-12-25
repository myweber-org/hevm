module WordFrequency where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = words $ map normalizeChar text
        normalized = map normalizeWord wordsList
        grouped = groupCounts normalized
    in take 10 $ sortOn (Down . snd) grouped
  where
    normalizeChar c
        | c `elem` ",.!?;:\"()[]{}" = ' '
        | otherwise = toLower c
    
    normalizeWord w = filter (`notElem` "'-") w
    
    groupCounts :: [String] -> [WordCount]
    groupCounts = foldr countHelper []
    
    countHelper :: String -> [WordCount] -> [WordCount]
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | w == word = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

analyzeText :: String -> IO ()
analyzeText input = do
    putStrLn "Top 10 most frequent words:"
    mapM_ printWord (countWords input)
  where
    printWord (word, count) = 
        putStrLn $ word ++ ": " ++ show countmodule WordFrequency where

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

topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
    take n $ List.sortBy (\(_, c1) (_, c2) -> compare c2 c1) $ Map.toList $ countWords text

displayFrequencies :: [(String, Int)] -> String
displayFrequencies freqList =
    unlines $ map (\(word, count) -> word ++ ": " ++ show count) freqList

analyzeText :: Int -> String -> String
analyzeText n text = displayFrequencies $ topNWords n text