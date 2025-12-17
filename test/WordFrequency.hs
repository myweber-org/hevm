module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = Map.Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w m = Map.insertWith (+) (normalize w) 1 m
    normalize = map toLower . filter isAlpha

topWords :: Int -> String -> [(String, Int)]
topWords n = take n . sortOn (Down . snd) . Map.toList . countWords

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  let top10 = topWords 10 content
  putStrLn "Top 10 most frequent words:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) top10