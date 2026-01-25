
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

countWords :: String -> Histogram
countWords text = 
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map (map toLower) wordsList
        grouped = group $ sort cleaned
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) grouped
  where
    clean = filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram hist = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') hist
    putStrLn $ "Total unique words: " ++ show (length hist)

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    input <- getContents
    let freq = countWords input
    printHistogram freq