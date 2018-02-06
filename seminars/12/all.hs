-- s
-- Lists
-- s
-- Да се дефинира функция elem(x, l), която проверява дали елемента x присъства в списъка l.

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x l
  | x == (head l) = True
  | otherwise = elem' x (tail l)

-- Да се дефинира функция reverse(l), която обръща списъка l.

reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = (last l) : (reverse' (init l))

-- Да се дефинира функция map(f, l), която прилага функцията f върху всеки елемент от списъка l.

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f l = f (head l) : map' f (tail l)
--map' f l = [ f x | x <- l]
-- map' (\x -> x**2) [1,2,3,4,5]

-- Да се дефинира функция filter(p, l), която връща списък с всички елементи на l, които удовлетворяват предиката p.
filter' :: (a -> Bool) -> [a] -> [a]
-- filter' p l = [ x | x <- l , p x]
filter' _ [] = []
filter' p l
  | p (head l) = head l : filter' p (tail l)
  | otherwise = filter' p (tail l)

-- Да се дефинира функция foldr(op, nullValue, l), която редуцира (акумулира) елементите
-- на списъка l чрез бинарната функция op до една стойност,
-- където nullValue е началната стойност на акумулатора.

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' _ nv [] = nv
foldr' op nv l = op (head l) (foldr' op nv (tail l))

-- Да се дефинира функция sort(l), която сортира списъка l.
-- Използвайте Quick Sort или друг любим алгоритъм за сортиране.
-- TODO
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
  let smaller = quicksort (filter' (< pivot) xs)
      bigger = quicksort (filter' (> pivot) xs)
  in smaller ++ [pivot] ++ bigger
-- s
-- matrices
-- s

-- Да се дефинира функция column(m, i), която намира i-тата по ред колона,
-- броейки от нула, в матрицата m.
column' :: [[a]] -> Int -> [a]
column' [] _ = []
column' m i = map (\row -> row !! i) m

row' :: [[a]] -> Int -> [a]
row' m i = m !! i

m = [[1,2,3], [4,5,6], [7,8,9]]

-- Да се дефинира функция transpose(m), която връща транспонираната матрица на m.

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' m = [ column' m i | i <- [0..(length m - 1)]]


-- Да се дефинира функция diagonal(m), която връща главния диагонал на матрицата m.
diagonal' :: [[a]] -> [a]
diagonal' [] = []
-- diagonal' m = (head (head m)) : diagonal' (tail (map tail m))
diagonal' (x:xs) = head x : diagonal' (map tail xs)
{-diagonal' m = iter 0 0
  where iter x y
          | (y == rows) || (x == cols) = []
          | otherwise = ((row' m y) !! x) : (iter (x + 1) (y + 1))
          where rows = length m
                cols = length (m !! 0)
-}
-- Да се дефинира функция diagonal2(m), която връща втория диагонал на матрицата m.
diagonal2' :: [[a]] -> [a]
diagonal2' [] = []
diagonal2' m = diagonal' (map reverse' m)

-- Да се дефинира функция foldm(op, nullValue, m), която редуцира (акумулира) елементите на матрицата m чрез бинарната функция op до една стойност, където nullValue е началната стойност на акумулатора.

-- TODO

-- s
-- sets
-- s

-- Да се дефинира функция intersection(a, b), която намира сечението на списъците a и b.
intersection' :: Eq a =>  [a] -> [a] -> [a]
intersection' [] _ = []
intersection' _ [] = []
intersection' a b = filter' (\e -> elem' e b) a

-- Да се дефинира функция difference(a, b), която намира разликата на двете множества
-- (списъци) a и b, като премахва срещанията на всички елементи на b от a.
difference' :: Eq a => [a] -> [a] -> [a]
difference' [] _ = []
difference' _ [] = []
difference' a b = filter' (\e -> not (elem' e b)) a

-- Да се дефинира функция product(a, b), която намира декартовото произведение
-- на двата списъка a и b и връща списък от наредени двойки.
product' :: [a] -> [a] -> [(a,a)]
product' [] _ = []
product' _ [] = []
product' a b = [ (x, y) | x <- a, y <- b]
---product [1, 2, 3] [4, 5]
-- Да се дефинира функция triplets(n), която връща списък от всички
-- Питагорови тройки със сума по-малка от n. TODO
triplets' :: Int -> [(Int, Int, Int)]
triplets' n = takeWhile (\(x,y,z) -> (x + y + z < n)) (filter (\(x,y,z) -> x^2 + y^2 == z^2) [(x, y, z) | x <- [1..], y <- [1..], z <- [1..]])




  --
  -- [ (x, y, z) | x <- [1..], y <- [1..], z <- [1..],
  --                           (x + y + z) < n && sq x + sq y == sq z]
  --                           where sq x = x*x
-- s
-- ass lists
-- s

-- Да се дефинира функция zip(a, b), която обединява поелементно списъците a и b
-- в списък от наредени двойки.
-- Например, zip [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd'] връща [(1, 'а'), (2, 'b'), (3, 'c'), (4, 'd')].
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' xs ys = []

-- Да се дефинира функция histogram(l), която връща хистограма на
-- срещанията на всички елементи в l под формата на списък от наредени двойки (кортежи).
count ::  Eq a => a -> [a] -> Int
count _ [] = 0
count e (x:xs)
  | x == e = 1 + count e xs
  | otherwise = count e xs

histogram :: Eq a => [a] -> [(a, Int)]
histogram [] = []
histogram l@(x:xs) = (x, count x l) : histogram (filter (\e -> e /= x) xs)
-- histogram [8, 7, 1, 7, 8, 2, 2, 8, 2, 7, 8, 1]

-- Да се дефинира функция runLengthEncode(l), която кодира списъка l в
-- асоциативен списък - списък от наредени двойки (<ключ>, <стойност>),
-- където <ключ>-ът e пореден елемент от списъка l, а <стойност>-та
-- е колко пъти се повтаря елемента последователно.
-- Например, runLengthEncode [8, 7, 7, 2, 2, 2, 2, 3, 3, 2] връща асоциативния
-- списък [(8, 1), (7, 2), (2, 4), (3, 2), (2, 1)].
runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode [] = []
runLengthEncode (x:xs)= (x, 1 + (countWhile (\e -> e == x) xs)) :
                        runLengthEncode (dropWhile (\e -> e == x) xs)
                        where countWhile _ [] = 0
                              countWhile p (x:xs)
                                | p x = 1 + countWhile p xs
                                | otherwise = 0
-- first solve
-- runLengthEncode (x:xs)= (x, 1 + (countNext x xs)) :
--                         runLengthEncode (removeTil (\e -> e /= x) xs)
--                         where countNext _ [] = 0
--                               countNext e (x:xs)
--                                 | x == e = 1 + countNext e xs
--                                 | otherwise = 0
--                               removeTil _ [] = []
--                               removeTil p l@(x:xs)
--                                 | p x = l
--                                 | otherwise = removeTil p xs
