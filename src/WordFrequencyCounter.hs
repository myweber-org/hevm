module WordFrequencyCounter where

import Data.Char (toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower

sortByFrequency :: WordCount -> [(String, Int)]
sortByFrequency = sortOn (Down . snd) . Map.toList

formatOutput :: [(String, Int)] -> String
formatOutput = unlines . map (\(word, count) -> word ++ ": " ++ show count)

analyzeText :: String -> String
analyzeText = formatOutput . sortByFrequency . countWords

main :: IO ()
main = do
    input <- getContents
    putStrLn $ analyzeText input