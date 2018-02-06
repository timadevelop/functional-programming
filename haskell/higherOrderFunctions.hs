-- by Vlad Timofeev, 2018-01-16.
-- all functions are written and after that verified :)

-- higher-order function is the function
-- returns or takes as parameter other function(s)

multThree :: (Num a) => a -> a -> a -> a
multThree a b c = a * b * c
-- " " is the procedure (call a function), so:
-- ((multThree applied to a) applied to b) applied to c
-- let a = 1, b = 1, c = 0
-- 1) multThree a returns f1 b c = 1 * b * c, f :: a -> a -> a
-- 2) f1 b returns f2 c = 1 * 1 * c, f :: a -> a
-- 3) f2 c returns result (1* 1 * 0) = 0
-- multTwoWithNine = multThree 9
-- multTwoWithNine 2 3 -> 54

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--   where g x y = f y x
flip' f x y = f y x -- when we call flip f -> function that takes args (x y) and applies them to f y x

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map' f l = [ f x | x <- l]


-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
-- sorting
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) =
  let smaller = quicksort (filter' (<= pivot) xs)
      bigger = quicksort (filter' (> pivot) xs)
  in smaller ++ [pivot] ++ bigger


-- currying
-- addThree :: (Num a) => a -> a -> a -> a
-- addThree x y z = x + y + z
-- addThree :: (Num a) => a -> a -> a -> a
-- addThree = \x -> \y -> \z -> x + y + z
flip2' :: (a -> b -> c) -> (b -> a -> c)
flip2' f = \x y -> f y x

-- s
-- folds and horses
-- s

-- foldl nv -> (\acc x)
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
-- sum = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
-- elem' _ [] = False
elem' e l = foldl (\acc x -> if x == e then True else acc) False l

-- foldr (\x acc) <- nv
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

--
mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs -- bad solution for map

--
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

--
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

--
filterf' :: (a -> Bool) -> [a] -> [a]
filterf' p xs = foldr (\x acc -> if p x then x:acc else acc) [] xs

--
product' :: (Num a) => [a] -> a
product' = foldr1 (*)

--
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- scanl -> [.., result], scanr -> [result, ...,...]

-- s
-- $ - the lowest precedence function application
-- s

-- ($') :: (a -> b) -> a -> b
-- f $' x = f x

-- $ is right-associative, so:
-- sqrt 3 + 5 + 1 --> ((sqrt 3) + 5) + 1
-- sqrt $ 3 + 5 + 1 --> (sqrt ((3 + 5) + 1))

-- the sum (filter (>10) (map (*3) [1..5]))
-- eq to:
-- sum $ filter (>10) $ map (*3) [1..5]

-- $ is a binary function, so :
-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- apply 3 to the list of functions
-- ($ 3) is a function that takes function
-- ghci> :t ($ 3)
-- ($ 3) :: Num a => (a -> b) -> b , Num is because 3 is a number

-- composition
compose' :: (b -> c) -> (a -> b) -> (a -> c)
compose' f g = (\x -> f $ g x)

doubleSquare = compose' (\x -> x*x) (\x -> 2*x)

map (negate . sum . tail) [[1,2,3], [3,4,5]]
-- for each sublist get tail ([2,3], [4,5]) , sum tail and finally negate it


-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- eq to
-- fn = ceiling . negate . tan . cos . max 50
