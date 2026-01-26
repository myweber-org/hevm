module WordFrequency where

import qualified Data.Map as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr updateCount Map.empty . words
  where
    updateCount word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

readFileAndCount :: FilePath -> IO [(String, Int)]
readFileAndCount path = do
    content <- readFile path
    return $ getTopWords 10 content

displayResults :: [(String, Int)] -> IO ()
displayResults = mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count)module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn, group, sort)
import Data.Ord (Down(..))

type WordCount = (String, Int)

histogramBar :: Int -> Int -> String
histogramBar count maxCount = replicate (scale count) '█'
  where
    scale c = round (fromIntegral c / fromIntegral maxCount * 50)

analyzeText :: String -> [WordCount]
analyzeText text = take 20 $ sortOn (Down . snd) wordCounts
  where
    words' = map normalize $ filter (not . null) $ splitText text
    wordCounts = map (\ws -> (head ws, length ws)) $ group $ sort words'
    
    normalize = map toLower . filter isAlpha
    splitText = words

printHistogram :: [WordCount] -> IO ()
printHistogram counts = do
    let maxCount = maximum $ map snd counts
    putStrLn "\nTop 20 Most Frequent Words:"
    putStrLn $ replicate 60 '─'
    mapM_ (\(word, count) -> 
        putStrLn $ padRight 15 word ++ " " ++ 
                 padLeft 4 (show count) ++ " " ++ 
                 histogramBar count maxCount) counts
    putStrLn $ replicate 60 '─'
  where
    padRight n str = str ++ replicate (n - length str) ' '
    padLeft n str = replicate (n - length str) ' ' ++ str

main :: IO ()
main = do
    putStrLn "Enter text to analyze (press Ctrl+D when finished):"
    content <- getContents
    let frequencies = analyzeText content
    printHistogram frequencies