-- Problem 3

m = [[1,2,3],
     [4,5,6],
     [7,8,9]] :: [[Int]] -- -> [](2,0) (0,2)]

m1 = [[9, 3, 1, 8, 0],
      [6, 5, 4, 6, 7],
      [2, 4, 4, 3, 8],
      [5, 6, 2, 2, 1]] :: [[Int]] -- > (1,2)

column :: Int -> [[Int]] -> [Int]
column _ [] = []
column i m = map (!! i) m

get :: [[a]] -> Int -> Int -> a
get matrix col row = (matrix !! row) !! col

hasSaddle :: [[Int]] -> Bool
hasSaddle m = 0 < (length $ allSeddles m)

-- bonus
-- imperative style in haskell :\

allSeddles :: [[Int]] -> [(Int, Int)]
allSeddles [] = []
allSeddles m = helper 0 0 m []
  where
    helper :: Int -> Int -> [[Int]] -> [(Int, Int)] -> [(Int, Int)]
    helper ci ri m acc -- column index, row index, matrix, last result
      | (ri >= length m) = acc -- outbound
      | (ci >= length (head m)) = helper 0 (ri + 1) m acc -- go to the next row
      | (minimum $ m !! ri) == (get m ci ri) &&
        (maximum (column ci m)) == (get m ci ri)
        = helper 0 (ri + 1) m ((ri, ci) : acc) -- found new seddle
      | (maximum $ m !! ri) == (get m ci ri) &&
        (minimum (column ci m)) == (get m ci ri)
        = helper 0 (ri + 1) m ((ri, ci) : acc) -- found new seddle
      | otherwise = helper (ci + 1) ri m acc -- there are no sadddles in current row
