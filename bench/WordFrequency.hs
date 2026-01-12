
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth = replicate barLength '█' ++ padding
  where
    barLength = round ((fromIntegral count / fromIntegral maxWidth) * 50)
    padding = replicate (50 - barLength) ' '

analyzeText :: String -> [WordCount]
analyzeText text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleanedWords = map (map toLower) wordsList
        grouped = group $ sort cleanedWords
        counts = map (\ws -> (head ws, length ws)) grouped
    in take 10 $ sortOn (Down . snd) counts
  where
    cleanWord = filter isAlpha

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = 
    let maxCount = maximum $ map snd counts
    in mapM_ (\(word, count) -> 
        putStrLn $ word ++ " " ++ histogramBar count maxCount ++ " " ++ show count) counts

processText :: String -> IO ()
processText input = do
    let topWords = analyzeText input
    putStrLn "\nTop 10 most frequent words:"
    displayHistogram topWords

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when done):"
    content <- getContents
    processText content