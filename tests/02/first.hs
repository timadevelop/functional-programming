l = [[1,1,3,2], [1,1,5], [1,5], [1,1,1,3]] -- 1
flatten :: [[a]] -> [a]
flatten = foldr (++) []


count :: (Eq a) => a -> [a] -> Integer
count _ [] = 0
count e (x:xs)
  | e == x = 1 + count e xs
  | otherwise = count e xs


mostFrequent1 :: Integer -> Integer -> [Integer] -> Integer
mostFrequent1 e _ [] = e
mostFrequent1 e eCount l
  | eCount < (count (head l) l) =
    mostFrequent1 (head l) (count (head l) l) (filter (/=(head l)) l)
  | otherwise = mostFrequent1 e eCount (filter (/=(head l)) l)


mostFrequent2 :: [[Integer]] -> Integer
mostFrequent2 [] = 0
mostFrequent2 xs =
  let l = flatten xs
  in mostFrequent1 (head l) (count (head l) l) (filter (/=(head l)) l)

mostFrequent :: [[Integer]] -> Integer
mostFrequent l = let cnts = map (\xs -> mostFrequent2 [xs]) l
                 in if all (== (head cnts)) cnts then (head cnts) else 0

l1 = [[1,1,3,2], [1,5,5], [1,5], [1,1,1,3]] -- 1
-- mostFrequent l -> 1
--- mostFrequent [[1], [2], [3]] -- --> 0
-- mostFrequent l1 -> 0
