module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type FrequencyMap = Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList $ countWords text

displayFrequency :: [(String, Int)] -> String
displayFrequency = unlines . map (\(w, c) -> w ++ ": " ++ show c)

analyzeText :: Int -> String -> String
analyzeText n = displayFrequency . topNWords n
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import Control.Arrow ((&&&))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth
    | maxWidth <= 0 = ""
    | otherwise = replicate barLength '█' ++ padding
  where
    scale = fromIntegral maxWidth / 50.0
    barLength = round (fromIntegral count / scale)
    padding = replicate (maxWidth - barLength) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    take 10 $ 
    sortOn (Down . snd) $
    map (head &&& length) $
    group $
    sort $
    filter (not . null) $
    map normalize $
    words text
  where
    normalize = filter isAlpha . map toLower

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = do
    putStrLn "\nTop 10 Most Frequent Words:"
    putStrLn "============================"
    
    let maxCount = maximum (map snd counts)
        maxWordLength = maximum (map (length . fst) counts)
    
    mapM_ (\(word, count) -> do
        let paddedWord = word ++ replicate (maxWordLength - length word + 2) ' '
            bar = histogramBar count maxCount
        putStrLn $ paddedWord ++ bar ++ " " ++ show count
        ) counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    let frequencies = analyzeText content
    displayHistogram frequencies