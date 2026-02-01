module WordFrequency where

import qualified Data.Map as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))

type FrequencyMap = Map.Map String Int

countWords :: String -> FrequencyMap
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord word = Map.insertWith (+) (normalize word) 1
    normalize = map toLower . filter isAlpha

getTopWords :: Int -> String -> [(String, Int)]
getTopWords n text = take n $ sortOn (Down . snd) $ Map.toList (countWords text)

main :: IO ()
main = do
    content <- readFile "sample.txt"
    let topWords = getTopWords 10 content
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) topWords