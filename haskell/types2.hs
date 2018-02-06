-- Record syntax

{-
data Person = Person String String Int Float String String deriving (Show)

firstName :: Person -> String
firstName (Person fn _ _ _ _ _) = fn

-- too much :/
lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
-}
-- is eq to :
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
-- now we have all these functions, try `:t firstName`
-- -> firstName :: Person -> String
-- with brackets the Show function is a little bit better

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
Car {company="Ford", model="Mustang", year=1967}
-- Use record syntax when a constructor has several fields and it's not obvious which field is which.
-- If we make a 3D vector data type by doing data Vector = Vector Int Int Int,
-- it's pretty obvious that the fields are the components of a vector.
-- However, in our Person and Car types, it wasn't so obvious and we greatly benefited from using record syntax.
-- learnyouhaskell.com
----
-- Location, in two dimensions.









class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p
