-- Problem 2

check :: [(Int -> Int)] -> [Int] -> Int
check _ [] = -1
check [] _ = -1
check (f:fns) (x:xs) = [ | ff <- [f]]


(f1 . f2 . f3) x == f4 x and > acc ->
