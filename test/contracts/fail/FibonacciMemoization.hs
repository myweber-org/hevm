import qualified Data.Map as Map

fib :: Int -> Integer
fib n = Map.findWithDefault (fib' n) n memoMap
  where
    memoMap = Map.fromList [(0, 0), (1, 1)]
    fib' 0 = 0
    fib' 1 = 1
    fib' x = fib (x - 1) + fib (x - 2)

main :: IO ()
main = do
    putStrLn "Fibonacci numbers:"
    mapM_ (print . fib) [0..20]