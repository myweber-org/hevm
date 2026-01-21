
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map

type Histogram = [(String, Int)]

analyzeText :: String -> Histogram
analyzeText text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        freqMap = foldr (\word -> Map.insertWith (+) word 1) Map.empty wordsList
        sorted = sortOn (Down . snd) $ Map.toList freqMap
    in take 10 sorted
  where
    normalize = map toLower . filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram hist = do
    putStrLn "\nTop 10 Word Frequencies:"
    putStrLn "========================"
    mapM_ (\(word, count) -> 
        putStrLn $ padRight 15 word ++ " | " ++ bar count ++ " " ++ show count
        ) hist
  where
    maxCount = maximum $ map snd hist
    bar n = replicate (n * 50 `div` maxCount) '█'
    padRight n str = take n $ str ++ repeat ' '

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    let histogram = analyzeText content
    printHistogram histogram