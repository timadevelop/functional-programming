module Shapes
( Point(..)
, Shape(..) -- if just Shape - user of module cannot
            -- use Circle and Rectangle value constructors, in pattern matching too.
            -- but user can use baseCircle as exported function which uses Cirle vc
, surface
, nudge
, baseCircle
, baseRect
) where

type Person = (String, String, Int)

getName :: Person -> String
getName (n, _, _) = n
{-

-- Shape is a type, Circle and Rectangle are value constructors
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- :t Circle
-- Circle :: Float -> Float -> Float -> Shape

-- we can use pattern mathing against value constructors:

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2-x1) * (abs $ y2-y1)

c = Circle 10 10 10
-- :t c -> c::Shape

-}

{-
-- show ?
-- let's make our type a part of Show typeclass
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- now haskell knows how to show out Shape in terminal

-- make 100 circles with center in (5,7) and radiuses 1,3,5,...99
-- because value constructors are functions, we can call them partially
circlesWithOddRadiuses n = map (Circle 5 7) [1,3..n]
-}

-- s
-- Improve shape
-- s
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surface $ Circle (Point 1 2) 10 -> 314.15...
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (x + dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy =
  Rectangle (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

-- nudge (Circle (Point 1 2) 1) 2 3 -> Circle (Point 3 4) 1

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
