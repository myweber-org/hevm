module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordFreq = (String, Int)

countWords :: String -> [WordFreq]
countWords text = map (\ws -> (head ws, length ws)) 
                  . group 
                  . sort 
                  . words 
                  . map normalizeChar 
                  $ text
  where
    normalizeChar c
      | isAlpha c = toLower c
      | otherwise = ' '

sortByFrequency :: [WordFreq] -> [WordFreq]
sortByFrequency = sortOn (Down . snd)

filterByMinFrequency :: Int -> [WordFreq] -> [WordFreq]
filterByMinFrequency minFreq = filter ((>= minFreq) . snd)

getTopNWords :: Int -> [WordFreq] -> [WordFreq]
getTopNWords n = take n . sortByFrequency

analyzeText :: String -> Int -> Int -> [WordFreq]
analyzeText text minFreq topN = 
  getTopNWords topN 
  . filterByMinFrequency minFreq 
  . countWords 
  $ text

displayResults :: [WordFreq] -> IO ()
displayResults freqList = do
  putStrLn "Word Frequency Analysis:"
  putStrLn "-------------------------"
  mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count) freqList
  putStrLn $ "Total unique words: " ++ show (length freqList)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        freqMap = foldr (\w m -> insertWord w m) [] cleaned
    in sortOn (Down . snd) freqMap
  where
    cleanWord = filter isAlpha
    insertWord w [] = [(w, 1)]
    insertWord w ((x, c):xs)
        | w == x = (x, c+1) : xs
        | otherwise = (x, c) : insertWord w xs

formatOutput :: [WordCount] -> String
formatOutput counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText = formatOutput . countWords

main :: IO ()
main = do
    input <- getContents
    putStr $ processText input