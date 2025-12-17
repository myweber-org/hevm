module WordFrequency where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlphaNum)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr insertWord Map.empty . words
  where
    insertWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlphaNum

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

processFile :: FilePath -> IO ()
processFile path = do
  content <- readFile path
  let topWords = getTopWords 10 content
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWords