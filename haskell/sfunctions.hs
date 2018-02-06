-- by Vlad Timofeev, 2018-01-16.
-- all functions are written and after that verified :)

head' :: [a] -> a
head' [] = error "list is empty"
head' (h:_) = h


-- pattern matching ;) - check pattern
tell :: (Show a) => [a] ->String
tell [] = "empty list"
tell (x:[]) = "List with element" ++ show x
tell (x:y:[]) = "list with 2 elements " ++ show x ++ "and " ++ show y
tell (x:y:_) = "List is too long. first 2 els are" ++ tell (x:y:[])

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

get_first :: String -> String
get_first [] = []
get_first all@(x:xs) = "The first of " ++ all ++ " is " ++ [x]
-- guards check boolean expr
grade :: (RealFloat a) => a -> String
grade x
  | x >= 5.5 = "Excellent"
  | x >= 4.5 = "Good"
  | x >= 3.0 = "Not bad"
  | otherwise= "Bad :(" -- if there is not otherwise and
-- no true in guards before - haskell will search next pattern.
--
--
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

--
--
reverse' :: [a] -> [a]
reverse' l
  | null l = l
  | otherwise = reverse' (tail l) ++ [head l]
 -- | otherwise = [last l] ++ reverse' (init l)

--- [1,2,3] !! 0 -> 1
--- elem 3 [1..5] -> true
--null [] = True
--null _ = False

--
--head (h:_) = h
--tail (_:t) = t
-- pyth
-- [ (x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x**2 + y**2 == z**2]
-- take 4 "helloo" -> hell
-- drop 2 [1,3..10] -> [5,7,9]
-- splitAt n l = (take n l, drop n l)
-- maximum
-- minimum
-- sum
-- product
-- and
-- or
-- concat :: [[a]] -> [a]
-- concat [[1,2,3], [4,5,7]] -> [1,2,3,4,5,7]
--
--
--LAMBDAS
--
id = \x -> x
sqrt = \x -> x**x



map' :: (a -> b) -> [a] -> [b]
map' f l = [ f x | x <- l ]
--map' _ [] = []
---map' f (x:xs) = f x : map' f xs
-- map (\x -> x*x) [1..4]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p l = [ x | x <- l, p x]
--filter (\x -> x < 10) [1..20]
