module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Histogram = [(String, Int)]

analyzeText :: String -> Histogram
analyzeText text = 
    let wordsList = filter (not . null) $ map normalize $ words text
        grouped = map (\ws -> (head ws, length ws)) $ group $ sort wordsList
    in take 10 $ sortOn (Down . snd) grouped
  where
    normalize = map toLower . filter isAlpha

printHistogram :: Histogram -> IO ()
printHistogram hist = do
    putStrLn "\nTop 10 most frequent words:"
    putStrLn "---------------------------"
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")") hist

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    let histogram = analyzeText content
    printHistogram histogram