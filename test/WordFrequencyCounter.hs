module WordFrequencyCounter where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map

type WordFreq = Map.Map String Int

countWords :: String -> WordFreq
countWords = foldr incrementWord Map.empty . words
  where
    incrementWord w = Map.insertWith (+) (normalize w) 1
    normalize = map toLower . filter isAlpha

topNWords :: Int -> String -> [(String, Int)]
topNWords n text = take n $ sortOn (negate . snd) $ Map.toList (countWords text)

displayFrequencies :: String -> IO ()
displayFrequencies text = do
  putStrLn "Word frequencies:"
  mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ topNWords 10 text