module WordFrequencyCounter where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a string
countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        groups = groupCount words'
    in take 10 $ sortOn (Down . snd) groups
  where
    cleanWord = map toLower . filter isAlpha
    groupCount = map (\ws -> (head ws, length ws)) . group . sort

-- | Group consecutive equal elements
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = 
    let (first, rest) = span (== x) xs
    in (x:first) : group rest

-- | Example usage function
exampleUsage :: IO ()
exampleUsage = do
    let sampleText = "Hello world hello haskell world programming haskell hello"
    print $ countWords sampleTextmodule WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordFrequency = Map String Int

countWords :: String -> WordFrequency
countWords text = foldr incrementWord Map.empty (words processedText)
  where
    processedText = map normalizeChar text
    normalizeChar c
        | isAlpha c = toLower c
        | otherwise = ' '

incrementWord :: String -> WordFrequency -> WordFrequency
incrementWord word freqMap = Map.insertWith (+) word 1 freqMap

getTopWords :: Int -> WordFrequency -> [(String, Int)]
getTopWords n freqMap = take n $ sortOn (\(_, count) -> negate count) (Map.toList freqMap)

processText :: String -> Int -> [(String, Int)]
processText text n = getTopWords n (countWords text)

displayResults :: [(String, Int)] -> String
displayResults results = unlines $ map (\(word, count) -> word ++ ": " ++ show count) results

main :: IO ()
main = do
    let sampleText = "Hello world! This is a test. Hello again, world!"
    let topWords = processText sampleText 3
    putStrLn "Top words:"
    putStrLn $ displayResults topWords