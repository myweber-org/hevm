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
    printHistogram histogrammodule WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        frequencies = foldr countWord [] cleaned
    in sortOn (Down . snd) frequencies
  where
    cleanWord = filter isAlpha
    countWord w [] = [(w, 1)]
    countWord w ((x, c):xs)
        | w == x = (x, c + 1) : xs
        | otherwise = (x, c) : countWord w xs

formatResults :: [WordCount] -> String
formatResults counts = 
    unlines $ map (\(w, c) -> w ++ ": " ++ show c) counts

processText :: String -> String
processText = formatResults . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ processText input