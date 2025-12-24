
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