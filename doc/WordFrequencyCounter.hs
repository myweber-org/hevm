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
    print $ countWords sampleText