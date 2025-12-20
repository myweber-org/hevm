module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

wordFrequency :: String -> Histogram
wordFrequency text = 
    let wordsList = filter (not . null) $ map (map toLower . filter isAlpha) $ words text
        grouped = group $ sort wordsList
        counts = map (\ws -> (head ws, length ws)) grouped
    in sortOn (Down . snd) counts

printHistogram :: Histogram -> IO ()
printHistogram freq = 
    mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ replicate count '*') freq

main :: IO ()
main = do
    putStrLn "Enter text to analyze:"
    input <- getContents
    let freq = wordFrequency input
    printHistogram freq