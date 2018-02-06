-- by Vlad Timofeev, 2018-01-16.
-- all functions are written and after that verified :)

-- var is a function

-- pyth n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y = z*z]

circumference' :: Double -> Double
circumference' r = 2 * pi * r

hypotenuse :: Double -> (Double -> Double) -- func takes 1 argument and returns other function that takes arg double and return double
hypotenuse a b = sqrt (a**2 + b**2)

--(hypotenuse 3) 4 -- here (hypotenuse 3) return a function F and F is applied to argument 4
-- curriyng
div50 :: Int -> Int
--div50 x = div 50 x -- fx = gx
div50 = div 50 -- f = g

twice f x = f (f x)
--twice :: (Int -> Int) -> Int -> Int
--or
twice :: (t -> t) -> t -> t
--twice div50 x --> x 50/(50/x
--
--
-- functions in haskell are prefix
-- operations are binary and infix but (-x)
--
-- functions with 2 arguments to binary operations using '<function>'
-- operations to functions : (+) , (<operation>)
inc = (+) 1

-- :t (2^) - from operations to one-argument function
-- square = (^2)
-- positive = (>0)
--
-- if <Bool> then t else t (same types!)
-- 1
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "bad input"

grade x
  | x >= 5.5 = "A"
  | x >= 5 = "B"
  | otherwise = "What?"
--- let
-- let x = 10 in x*2 -> 20
-- (100 `div`) --> function :: Int -> Int (= 100 `div` 10)
-- (`div` 100) -> function :: Int -> INt (= x `div` 100)
--
