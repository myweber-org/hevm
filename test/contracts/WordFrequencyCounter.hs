module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type Word = String

countWordFrequencies :: [Word] -> [(Word, Int)]
countWordFrequencies words =
  let normalized = map (map toLower) words
      sorted = sort normalized
      grouped = group sorted
      frequencies = map (\ws -> (head ws, length ws)) grouped
      sortedFrequencies = sortOn (Down . snd) frequencies
  in sortedFrequencies

filterByMinimumFrequency :: Int -> [(Word, Int)] -> [(Word, Int)]
filterByMinimumFrequency minFreq = filter (\(_, count) -> count >= minFreq)

main :: IO ()
main = do
  let sampleText = ["the", "quick", "brown", "fox", "jumps", "over", 
                    "the", "lazy", "dog", "the", "fox", "fox"]
      frequencies = countWordFrequencies sampleText
      filtered = filterByMinimumFrequency 2 frequencies
  
  putStrLn "All word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) frequencies
  
  putStrLn "\nWords appearing at least 2 times:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) filtered