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
  putStrLn $ "Total unique words: " ++ show (length freqList)