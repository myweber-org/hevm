module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) words'
    in sortOn (Down . snd) $ map (\ws -> (head ws, length ws)) $ group $ sort cleaned
  where
    cleanWord = map toLower . filter (\c -> isAlpha c || c == '\'')

printHistogram :: [WordCount] -> IO ()
printHistogram counts = do
    putStrLn "Word Frequency Histogram:"
    putStrLn "=========================="
    mapM_ printBar counts
  where
    printBar (word, count) = 
        putStrLn $ word ++ ": " ++ replicate count '*' ++ " (" ++ show count ++ ")"
    
    maxWordLength = maximum $ map (length . fst) counts

analyzeText :: String -> IO ()
analyzeText text = do
    let frequencies = countWords text
    putStrLn $ "Total unique words: " ++ show (length frequencies)
    putStrLn $ "Most frequent word: " ++ fst (head frequencies)
    printHistogram $ take 10 frequencies