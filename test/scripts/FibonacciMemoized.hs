fib :: Int -> Integer
fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  putStrLn "Fibonacci numbers from 0 to 20:"
  mapM_ (print . fib) [0..20]