module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (negate . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map formatPair
  where
    formatPair (word, count) = word ++ ": " ++ show count

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText input