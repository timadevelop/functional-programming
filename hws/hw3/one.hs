-- Problem 1

-- botLeft, topRight
-- (x1, y1, x2, y2)
type Rect = (Integer,Integer,Integer,Integer)

l = [(4,3,11,8),(7,0,13,6),(2,2,8,5),(0,4,6,7)] -- -> (4,3,11,8)

--  rect getters

x1 :: Rect -> Integer
x1 (x, _, _, _) = x
y1 :: Rect -> Integer
y1 (_, x, _, _) = x
x2 :: Rect -> Integer
x2 (_, _, x, _) = x
y2 :: Rect -> Integer
y2 (_, _, _, x) = x

-- helpers
noOverlap :: Rect -> Rect -> Bool
noOverlap r1 r2 = x1 r1 > x2 r2 ||
                  x1 r2 > x2 r1 ||
                  y1 r1 > y2 r2 ||
                  y1 r2 > y2 r1

intersects :: Rect -> Rect -> Bool
intersects r1 r2 = not $ noOverlap r1 r2

-- for each rect count intersections with other rectangles in list
countIntersections :: [Rect] -> [(Rect, Int)]
countIntersections l =
  foldr (\x acc -> (x , intersectionsOfIn x l):acc) [] l
  where intersectionsOfIn :: Rect -> [Rect] -> Int
        intersectionsOfIn x l = length $ (filter (\y -> intersects x y) l)

-- take rectangles with max count of intersections
filterByMaxIntersections :: [(Rect, Int)] -> [(Rect, Int)]
filterByMaxIntersections l = foldr helper [] l
  where helper x [] = [x]
        helper x acc
          | (snd x) > (snd (head acc)) = [x]
          | (snd x) == (snd (head acc)) = x:acc
          | otherwise = acc

-- surface function
surface :: Rect -> Integer
surface r = (abs $ x1 r - x2 r) * (abs $ y1 r - y2 r)

-- take Rectangles with max surface
filterByMaxSurface :: [(Rect, Int)] -> [(Rect, Int)]
filterByMaxSurface l = foldr helper [] l
  where helper x [] = [x]
        helper x acc
          | surface (fst x) > surface (fst $ head acc) = [x]
          | surface (fst x) == surface (fst $ head acc) = x:acc
          | otherwise = acc

-- countIntersections -> filterByMaxIntersections -> filterByMaxSurface -> get fisrt pair (Rect, Int) -> get Rect from pair
mostPopular :: [Rect] -> Rect
mostPopular = fst . head . filterByMaxSurface . filterByMaxIntersections . countIntersections
-- mostPopular rs = fst $ head $ filterByMaxSurface $ filterByMaxIntersections $ countIntersections rs
