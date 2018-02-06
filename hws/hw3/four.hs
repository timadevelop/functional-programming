-- Problem 4
-- 
reprs = [ countPossible i 2 | i <- [0..]]

countPossible :: Integer -> Integer -> Integer
countPossible _ 1 = 1
countPossible n k
  | n == k = 1
  | k > n = 0
  | otherwise = (countPossible (n - 1) ( k - 1)) +
                (countPossible (n - k) k)
