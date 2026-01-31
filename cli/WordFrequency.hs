module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr insertWord Map.empty . words
  where
    insertWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (Down . snd) $ Map.toList $ countWords text

readAndAnalyze :: FilePath -> IO ()
readAndAnalyze filepath = do
    content <- readFile filepath
    let topWords = topNWords 10 content
    putStrLn "Top 10 most frequent words:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWords