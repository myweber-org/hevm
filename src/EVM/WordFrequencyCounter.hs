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

displayHistogram :: [WordCount] -> String
displayHistogram counts = unlines $ map showRow counts
  where
    maxCount = maximum (map snd counts)
    scale = 50
    showRow (word, count) = 
        let barLength = round (fromIntegral count / fromIntegral maxCount * scale)
            bar = replicate barLength '█'
        in word ++ ": " ++ bar ++ " " ++ show count

analyzeText :: String -> String
analyzeText text = 
    let counts = countWords text
        totalWords = sum (map snd counts)
        uniqueWords = length counts
    in "Total words: " ++ show totalWords ++ "\n" ++
       "Unique words: " ++ show uniqueWords ++ "\n\n" ++
       "Word frequency histogram:\n" ++ displayHistogram (take 10 counts)

-- Example usage (commented out for library use)
-- main :: IO ()
-- main = do
--     let sampleText = "Hello world hello haskell world programming haskell functional programming"
--     putStrLn $ analyzeText sampleText