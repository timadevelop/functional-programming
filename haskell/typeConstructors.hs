-- Type parameters & Type constructors

-- produces new types, as templates in c++

data Maybe' a = Nothing' | Just' a deriving (Show)
-- Maybe' is a TYPE CONSTRUCTOR.
-- but Maybe' [Char], Maybe <Type>.. are types

-- a is a type parameter
-- Just' "ds"-> Just' "ds" :: Maybe' [Char]

-- :t Nothing'
-- -> Maybe' a (polymorphic type)

data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)

-- tellCar :: Car -> String does NOT work, Car is not a type, its type constructor

-- TODO
-- read section


-- derive (Eq) -> ownType == ownType

-- type synonym

-- type keyword:
-- type String = [Char]
-- [] is a type constructor, [Char] is a type
