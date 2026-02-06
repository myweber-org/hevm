module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

-- | Count frequency of each word in a text
countWords :: String -> [WordCount]
countWords text = 
    let words' = filter (not . null) $ map cleanWord $ words text
        cleaned = filter (all isAlpha) words'
    in map (\ws -> (head ws, length ws)) 
       $ group 
       $ sort 
       $ map (map toLower) cleaned
  where
    cleanWord = takeWhile isAlpha . dropWhile (not . isAlpha)

-- | Display word frequencies as a simple histogram
displayHistogram :: [WordCount] -> IO ()
displayHistogram counts = 
    mapM_ (\(word, count) -> 
        putStrLn $ word ++ ": " ++ replicate count '*') 
    $ take 20 
    $ sortOn (Down . snd) counts

-- | Main analysis function
analyzeText :: String -> IO ()
analyzeText text = do
    putStrLn "Top 20 most frequent words:"
    displayHistogram $ countWords text
    let total = sum $ map snd $ countWords text
    putStrLn $ "\nTotal words analyzed: " ++ show total

-- | Example usage
exampleAnalysis :: IO ()
exampleAnalysis = do
    let sampleText = "Hello world! This is a test. Hello again world. Testing testing one two three."
    analyzeText sampleText