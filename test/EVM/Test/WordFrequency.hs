module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

displayFrequencies :: [(String, Int)] -> String
displayFrequencies = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequencies . topNWords n
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth
    | maxWidth <= 0 = ""
    | otherwise = replicate barLength '█' ++ padding
  where
    barLength = round (fromIntegral count / fromIntegral maxWidth * 30)
    padding = replicate (30 - barLength) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    take 10 $ 
    sortOn (Down . snd) $
    map (\ws -> (head ws, length ws)) $
    group $
    sort $
    filter (not . null) $
    map (filter isAlpha . map toLower) $
    words text

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = do
    putStrLn "\nTop 10 Most Frequent Words:"
    putStrLn "============================="
    let maxCount = maximum (map snd counts)
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ " " ++ histogramBar count maxCount ++ " " ++ show count) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    let frequencies = analyzeText content
    displayHistogram frequencies