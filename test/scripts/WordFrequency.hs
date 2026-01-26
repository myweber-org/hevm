module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map

type WordCount = Map String Int

countWords :: String -> WordCount
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = filter isAlpha . map toLower

topWords :: Int -> String -> [(String, Int)]
topWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequency :: String -> IO ()
displayFrequency text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) (topWords 10 text)