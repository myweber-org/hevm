module WordFrequencyCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

countWords :: String -> Map String Int
countWords text = Map.fromListWith (+) wordCounts
  where
    words' = filter (not . null) (map cleanWord (words text))
    cleanWord = map toLower . filter isAlpha
    wordCounts = map (\w -> (w, 1)) words'

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n (sortOn (\(_, count) -> negate count) (Map.toList (countWords text)))

displayResults :: [(String, Int)] -> String
displayResults results = unlines (map (\(word, count) -> word ++ ": " ++ show count) results)

main :: IO ()
main = do
    let sampleText = "Hello world! Hello Haskell. Haskell is fun. World of Haskell."
    putStrLn "Word frequencies:"
    putStrLn (displayResults (topWords 5 sampleText))