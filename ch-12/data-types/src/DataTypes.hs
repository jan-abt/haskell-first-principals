
module DataTypes
    ( 
    ) where


data Price = 
-- (a)
 Price Integer
-- (b)  [1]
 deriving (Eq, Show)

-- type constructor (a)
-- data constructor (b)
-- type argument [1]


data Manufacturer = 
--       (c)
         Mini
--        (d)i
       | Mazda 
--        (e)
       | Tata 
--        (f)
 deriving (Eq, Show) 

-- one type constructor (c)
-- three data constructors (d), (e), and (f)


data Seats =  Seats Int
 deriving (Eq, Show)

data Airline =
--    (g)
     PopuAir Seats 
--     (h)
 | CatapultsR'Us Seats 
--    (i)
 | TakeYourChancesUnited Seats 
--    (j)
 deriving (Eq, Show) 

-- one type constructor (g)
-- three data constructors (h), (i), and (j)

data Vehicle =
--     (k)
    Car Manufacturer Price 
--  (l)    [2]        [3]
 | Plane Airline 
--  (m)    [4]
 deriving (Eq, Show)

-- type constructor (k)
-- two data constructors (l) and (m).
-- type arguments [2], [3] and [4]
-- type arguments to (l) are [2] and [3]
-- type argument to (m) is [4]

--
-- =====================================================
--

myCar   = Car Mini (Price 14000) 
yourCar = Car Mazda (Price 20000) 
someCar = Car Tata (Price 7000)
doge    = Plane (PopuAir (Seats 100))

isPlane :: Vehicle -> Bool 
isPlane v = case v of
   Plane a   -> True
   Car m p   -> False

hasOnlyCars :: [Vehicle] -> Bool
hasOnlyCars [] = False
hasOnlyCars xs = 
 foldl (\a e -> 
  case e of 
   Plane p -> False
   _       -> a
 ) True xs


carsOrNot :: [Vehicle] -> [Bool] 
carsOrNot xs = 
 foldr (\e a ->
  case e of
  Car m p -> True:a
  _       -> False:a
 ) [] xs

getManu :: Vehicle -> Manufacturer 
getManu (Car m p) = m
getManu (Plane a) = error "The data constructor for this vehicle type does not know of a type argument called `Manufacturer`"

seatCount :: Vehicle -> Int
seatCount (Plane (PopuAir (Seats s))) = s
seatCount (Plane (CatapultsR'Us (Seats s))) = s
seatCount (Plane (TakeYourChancesUnited (Seats s))) = s

--
-- =============================================================
--

data Sample a = IntSample Int | StringSample String deriving Show

class TooMany a 
 where tooMany :: a -> Bool

instance TooMany Int 
 where tooMany n = n > 42

instance TooMany String
 where tooMany s = length s > 42

instance TooMany (Int, String) 
 where tooMany (n, s) = tooMany (n + length s) 

instance (Num a, TooMany a) => TooMany(a, a) 
 where tooMany (n, t) = tooMany (n + t)

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- language pragma "GeneralizedNewtypeDeriving":
-- with that,  we don’t have to define an instance of TooMany for Goats that’s merely identical to the Int instance. 
-- We can reuse the instance that we already have.

-- instance TooMany Goats where tooMany (Goats n) = n > 43

--
-- ==========================================================
--

data Person = Person { name :: String, age :: Int } deriving (Eq, Show)
-- usage:  
--  >Person "Papu" 5
--  >Person {name = "Papu", age 5}
--  >let papu = Person "Papu" 5
--  >age papu
--  >5
--  >name papu
--  >"Papu"
