-- ID:      FIB
-- Name:    Rabbits and Recurrence relations
-- Author:  Samuel Jackson
-- Email:   samueljackson@outlook.com
-- Description: computes rabbit pairs over n months, with each rabbit producing k pairs.

rabbitPairs :: Int -> Int -> Int
rabbitPairs n k = last $ take n $ fibs k
  where
    --compute the fibonacci sequence Fn = Fn-1 + k*Fn-2
    fibs :: Int -> [Int]
    fibs k = map fst $ iterate (\(a,b) -> (b,a*k+b)) (1,1)
