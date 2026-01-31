module FibonacciMemoized where

import qualified Data.Map as Map

fibMemo :: Int -> Integer
fibMemo = let fibMap = Map.fromList [(0,0), (1,1)]
              fib' n = case Map.lookup n fibMap of
                         Just val -> val
                         Nothing -> let val = fib' (n-1) + fib' (n-2)
                                    in Map.insert n val fibMap `seq` val
          in \n -> snd $ until (\(i,_) -> i >= n) 
                               (\(i,m) -> (i+1, fib' (i+1))) 
                               (0, fibMap)