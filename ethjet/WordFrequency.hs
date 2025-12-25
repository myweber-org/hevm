
module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxWidth = replicate (count * maxWidth `div` maxCount) '█'
  where maxCount = max 1 count

analyzeText :: String -> [WordCount]
analyzeText text = 
  let wordsList = filter (not . null) $ map (filter isAlpha . map toLower) $ words text
      grouped = map (\ws -> (head ws, length ws)) $ group $ sort wordsList
  in take 10 $ sortOn (Down . snd) grouped

displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = do
  putStrLn "\nTop 10 Most Frequent Words:"
  putStrLn "============================="
  let maxCount = maximum (map snd counts)
  mapM_ (\(word, count) -> 
    putStrLn $ word ++ " " ++ replicate (12 - length word) '.' ++ 
               " " ++ show count ++ " " ++ histogramBar count 20) counts

main :: IO ()
main = do
  putStrLn "Enter text to analyze (press Ctrl+D when done):"
  content <- getContents
  let frequencies = analyzeText content
  displayHistogram frequencies
module WordFrequency where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = [(String, Int)]

countWords :: String -> WordCount
countWords text = 
    let wordsList = filter (not . null) $ map cleanWord $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr countHelper [] cleaned
    in sortOn (Down . snd) grouped
  where
    cleanWord = filter isAlpha
    countHelper word [] = [(word, 1)]
    countHelper word ((w, c):rest)
        | word == w = (w, c + 1) : rest
        | otherwise = (w, c) : countHelper word rest

topNWords :: Int -> String -> WordCount
topNWords n = take n . countWords

wordFrequencyReport :: String -> String
wordFrequencyReport text = 
    let counts = countWords text
        total = sum $ snd <$> counts
        reportLines = map (\(w, c) -> w ++ ": " ++ show c ++ " (" ++ showPercent c total ++ "%)") counts
    in unlines reportLines
  where
    showPercent count total' = 
        let percent = (fromIntegral count / fromIntegral total' * 100) :: Double
        in show (round percent :: Int)