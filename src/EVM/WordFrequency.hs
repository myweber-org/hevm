
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map (map toLower) wordsList
        grouped = group $ sort cleaned
        counted = map (\ws -> (head ws, length ws)) grouped
    in take 10 $ sortOn (Down . snd) counted
  where
    clean = filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram freq = 
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') freq

analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 10 most frequent words:"
    printHistogram $ wordFrequency text