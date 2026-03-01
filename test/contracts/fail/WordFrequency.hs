module WordFrequency where

import Data.Char (toLower, isAlpha)
import Data.List (sortOn)
import Data.Ord (Down(..))

type WordCount = (String, Int)

countWords :: String -> [WordCount]
countWords text =
    let wordsList = filter (not . null) $ map clean $ words text
        cleaned = map toLower <$> wordsList
        grouped = foldr (\w m -> insertWith (+) w 1 m) empty cleaned
    in sortOn (Down . snd) $ toList grouped
  where
    clean = filter isAlpha

    -- Simple association list implementation for demonstration
    data AssocList k v = Empty | Node k v (AssocList k v)
    
    empty :: AssocList k v
    empty = Empty
    
    insertWith :: (Eq k) => (v -> v -> v) -> k -> v -> AssocList k v -> AssocList k v
    insertWith _ k v Empty = Node k v Empty
    insertWith f k v (Node k' v' rest)
        | k == k'   = Node k (f v v') rest
        | otherwise = Node k' v' (insertWith f k v rest)
    
    toList :: AssocList k v -> [(k, v)]
    toList Empty = []
    toList (Node k v rest) = (k, v) : toList rest

-- Example usage function
printWordFrequencies :: String -> IO ()
printWordFrequencies text = do
    putStrLn "Word frequencies:"
    mapM_ (\(w, c) -> putStrLn $ w ++ ": " ++ show c) $ countWords text