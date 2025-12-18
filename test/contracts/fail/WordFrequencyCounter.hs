module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
        grouped = group $ sort wordsList
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped

printHistogram :: [WordCount] -> IO ()
printHistogram counts = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") 
        counts

main :: IO ()
main = do
    putStrLn "Enter text to analyze word frequency:"
    input <- getLine
    let frequencies = countWords input
    printHistogram frequencies