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
displayResults = mapM_ (\(word, count) -> putStrLn $ word ++ ": " ++ show count)