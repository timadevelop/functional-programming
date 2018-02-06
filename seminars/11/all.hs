--- add.hs
add :: Num a => a -> a -> a
--add x y = (+) x y
add = (+)

--- add-n
addN :: Num a => a -> (a -> a)
addN = (+)

---signum
signum' :: Integral a => a -> a
signum' x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

--- factorial
factorial :: Integral a => a -> a
--factorial n = product [1..n]
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- fibonacci
fibonacci :: Integral a => a -> a
{-fibonacci 0 = 0
fibonacci 1 = 0
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-}
fibonacci n = iter 0 1 n
  where iter current _ 0 = current
        iter current next n = iter next (current + next) (n - 1)
-- sum
sum' :: Integral a => a -> a -> a
sum' s e
  | s > e  = 0
  | otherwise = s + sum' (s+1) e

-- count digits
countDigits :: Integral a => a -> a
countDigits 0 = 1
countDigits n = 1 + countDigits (div n 10)

-- repeat
repeat' :: (a -> a) -> Int -> (a -> a)
repeat' f 1 = (\ x -> f x)
repeat' f n = (\ x -> f ((repeat' f (n-1)) x))

square x = x**2
-- accumulate

accumulate :: (b -> a -> b) -> b -> (Int -> a) ->
              Int -> (Int -> Int) -> Int -> b

accumulate combiner nullValue term a next b
  | a > b     = nullValue
  | otherwise = accumulate combiner (combiner nullValue (term a))
                term (next a) next b
--accumulate (+) 0 (* 2) 1 (\x -> x + 1) 5
-- eq
--sum [x * 2 | x <- [1..5]]

{-
accumulate' :: (b -> a -> b) -> b -> (a -> b) ->
              Int -> (Int -> b) -> Int -> b


accumulate' combiner nullValue term a next b
  | a > b = nullValue
  | otherwise = accumulate combiner (combiner nullValue (term a)) term (next a) next b
--  | otherwise = combiner (term a) (accumulate combiner nullValue term (next a) next b)
-}
