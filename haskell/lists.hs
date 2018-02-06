{-
maximum :: Ord a => [a] -> a — максимален елемент
minimum :: Ord a => [a] -> a — минимален елемент
sum :: Num a => [a] -> a — сума на списък от числа
product :: Num a => [a] -> a — произведение на списък от числа
and :: [Bool] -> Bool — конюнкция на булеви стойности
or :: [Bool] -> Bool — дизюнкция на булеви стойности
concat :: [[a]] -> [a] — конкатенация на списък от списъци
takeWhile p [a]
dropWhile p [a]
span p [a] -> (takeWhile , dropWhile)
break p [a] -> span (not p) [a] ?
-}
