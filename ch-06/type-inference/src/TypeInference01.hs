
-- TypeInference01.hs
module TypeInference01 where 

 {-
 -  
 -  f :: (Num a) => a  ->  b  ->  Int  ->  Int
 -                 [0]    [1]     [2]      [3]
 -
 -    constrained polymorphic (Num) :  ([0]) 
 -    fully polymorphic             :  ([1]) 
 -    concrete                      :  ([2] and [3])
 -
 -}


-- validate, that type inference won't change regardless of 
-- what kind of numbers are passed to the parameters as input arguments passed to the function
-- because type constraints have been specified.
func1 :: Fractional a => a -> a -> a
func1 x y = x + y + 3

-- validate that type inference will change depending on 
-- what kind of numbers are passed to the parameters as input arguments passed to the function
-- because there aren't any type contraints. 
func2 x y = x + y + 3

--          [f1]        [f2]        [f3]
func3 :: (b -> c) -> (a -> b) -> (a -> c)
-- 1. apply [f2] to the argument passed as a parameter to the lambda [f3].
-- 2. apply [f1] to the output resulting from step 1.
-- 3. return the output resulting from step 2.
func3 f g = \a ->  f    (g a)
--          |            [f2]--|
--          |       [f1]-------|
--          |[f3]--------------|

f1 ::  (a -> b) -> a -> b
f1 f x = f x 

-----------------------------

data Woot = W deriving (Show)
data Blah = B deriving (Show)

a1 :: Woot -> Blah
a1 w = B

a2 :: (Blah, Woot) -> (Blah, Blah) 
a2 (b, w) = (b, a1 w)

a3 :: Int -> String 
a3 = undefined

a4 :: String -> Char 
a4 = undefined

a5 :: Int -> Char
a5  n = a4 (a3 n)

---------------------------

data A
data B
data C

b1 :: A -> B
b1 = undefined

b2 :: B -> C
b2 = undefined

b3 :: A -> C
b3 a = b2 (b1 a)

--------------------------

data X
data Y
data Z

c1 :: X -> Z
c1 = undefined

c2 :: Y -> Z
c2 = undefined

c3 :: (X, Y) -> (Z, Z)
c3 (x,y) = (c1 x, c2 y)

--------------------------

d1 :: (x -> y) -> (y -> (w, z)) -> x -> w
d1 f g x =  let (w, _) = g (f x)
            in w


