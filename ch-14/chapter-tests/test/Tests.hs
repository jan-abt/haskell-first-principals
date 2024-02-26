module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List
import Data.Char

-- cabal v2-test chapter-tests
-- Hspec
main :: IO () 
main = hspec $ do
 describe "unit test" $ do 
  it "(id 0) == 0" $ do
   id 0 `shouldBe` 0
 describe "quick check" $ do 
  it "(2 * half n) == (id n)" $ do
   property $ prop_half 
  it "order: previous >= current" $ do
   property $ prop_listOrdered 
  it "additive associativity: x + (y + z) == (x + y) + z" $ do
   property $ prop_plusAssociative
  it "additive commutativity: x + y == y + x" $ do
   property $ prop_plusCommutative
  it "multiplicative associativity: x + (y + z) == (x + y) + z" $ do
   property $ prop_multiplyAssociative
  it "multiplicative commutativity: x + y == y + x" $ do
   property $ prop_multiplyCommutative
  it "quotient remainder relationship: y /= 0 ==> (quot x y)*y + (rem x y) == x" $ do
   property $ prop_quotientRemainderRelationship
  it "division modulus relationship: y /= 0 ==> (div x y)*y + (mod x y) == x" $ do
   property $ prop_divisionModulusRelationship
  it "Associativity Test (discarding division by zero)" $ do
   property $ quickCheck prop_associativity
  it "Commutativity Test (discarding division by zero)" $ do
   property $ quickCheck prop_commutativity
  it "Reversal Test " $ do
   property $ quickCheck prop_reverse
  it "Function Composition Test 1" $ do
   property $ quickCheck prop_composition1
  it "Function Composition Test 2" $ do
   property $ quickCheck prop_composition2
  it "Function Composition Test 3" $ do
   property $ quickCheck prop_composition3
  it "Function Application with $ Test" $ do
   property $ quickCheck prop_dollar
  it "foldr and (:) Test" $ do
   property $ quickCheck prop_foldrAndCons
  it "(++) and (:) Test" $ do
   property $ quickCheck prop_plusPlusAndCons
  it "(++) and concat Test" $ do
   property $ quickCheck prop_plusPlusAndConcat
  it "length and take Test" $ do
   property $ quickCheck prop_lengthAndTake
  it "show and read Test" $ do
   property $ quickCheck prop_showAndRead 
  it "square identity Test" $ do
   property $ quickCheck prop_squareRootAndSquare 
  it "capitalizeWord idempotence Test" $ do
   property $ quickCheck prop_capitalizeWordIdempotence 
  it "sort idempotence Test" $ do
   property $ quickCheck prop_sortIdempotence 










-- cabal repl tests 
-- QuickCheck

-- our function
half :: Double -> Double
half x = x /  2 

-- this property should hold
prop_half :: Double -> Bool
prop_half n  =  id n == ((half . (*2)) $ n) 

--halfIdentityQC :: IO ()
--halfIdentityQC = quickCheck prop_half


-- for any list you apply sort to this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where 
   go _  acc@(_, False) = acc
   go el (Nothing, t) = (Just el, t)
   go el (Just prevEl, _) = (Just el, prevEl >= el)

prop_listOrdered ::  [Int] -> Bool
prop_listOrdered xs = listOrdered (sort xs) 

prop_plusAssociative:: Int -> Int -> Int -> Bool -- (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative:: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

prop_multiplyAssociative:: Int -> Int -> Int -> Bool
prop_multiplyAssociative x y z = x * (y * z) == (x * y) * z

prop_multiplyCommutative:: Int -> Int -> Bool
prop_multiplyCommutative x y = x * y == y * x

-- The property is only checked when y is not equal to zero, thus avoiding division by zero.
prop_quotientRemainderRelationship :: Int -> Int -> Property
prop_quotientRemainderRelationship x y =
  y /= 0 ==> (quot x y) * y + (rem x y) == x

-- The property is only checked when y is not equal to zero, thus avoiding division by zero.
prop_divisionModulusRelationship :: Int -> Int -> Property
prop_divisionModulusRelationship x y =
 y /= 0 ==>  (div x y)*y + (mod x y) == x

-- Associativity
-- The property is only checked when y is not equal to zero, thus avoiding division by zero.
prop_associativity :: Integer -> Integer -> Property
prop_associativity x y = 
 y /= 0 ==>
  (quot x y) * y + rem x y == x &&
  (div x y) * y + mod x y == x

-- Commutativity
-- The property is only checked when y is not equal to zero, thus avoiding division by zero.
prop_commutativity :: Integer -> Integer -> Property
prop_commutativity x y = 
  y /= 0 ==> 
   (quot x y) * y + rem x y == (div x y) * y + mod x y

prop_reverse :: [Integer] -> Bool
prop_reverse xs  = (reverse . reverse) xs == id xs

-- ======================  function property tests ==========================

-- Opt. 1/3
-- using  QuickCheck's Fun type
prop_composition1 :: Fun Int Int  -> Fun Int Int  -> Int -> Bool
prop_composition1 f g x = ( (apply f) . (apply g) ) x == apply f (apply g x)

-- Opt. 2/3
-- wrap our (a -> a) type in a newtype ... 
newtype UnaryFunc a =  UnaryFunc (a -> a) deriving (Arbitrary)

-- ... and  declare a Show instance on the new type
instance Show (UnaryFunc a) where
  show _ = "a -> a"

prop_composition2 :: UnaryFunc Int -> UnaryFunc Int -> Int -> Bool
prop_composition2 (UnaryFunc f) (UnaryFunc g) x = (f . g) x == f (g x)

-- Opt. 3/3
-- using (Int -> Int)

-- We have to write instance of Show for (Int -> Int)
instance Show (Int -> Int) where
  show _ = "Int -> Int"

prop_composition3 :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
prop_composition3 f g  x = (f . g) x == f (g x)

-- onwards with more tests ...
prop_dollar :: (Int -> Int) -> Int -> Bool
prop_dollar f n = (f $ n) == f n

prop_foldrAndCons :: [Int] -> Bool
prop_foldrAndCons xs = xs == ( (foldr (:) []) . ([] ++) $ xs)

prop_plusPlusAndCons :: Int -> Bool
prop_plusPlusAndCons x = [x] == ( ( (++) [] ) . ( flip (:) [] ) ) x

prop_plusPlusAndConcat :: [[Int]] -> Bool
prop_plusPlusAndConcat xss = (foldr (++) [] xss) == ( concat xss )  

prop_lengthAndTake :: Int -> [Int] -> Property
prop_lengthAndTake n xs =  n >=0 && n <= length xs  ==> length (take n xs) == n

prop_showAndRead :: Int -> Bool
prop_showAndRead n = (read (show n)) == n


--  When we apply  sqrt to a value and then square the result,
--  we may not always get back exactly the original value due to rounding errors in floating-point arithmetic.
--  Floating-point numbers have limited precision and so certain values cannot be represented exactly.
square :: Double -> Double
square x = x * x

boundary :: Double
boundary = 1e-10

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < boundary

prop_squareRootAndSquare :: Double -> Property
prop_squareRootAndSquare n = n >= 0 ==> approxEqual (square (sqrt n)) n

-- Idempotence
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = ((toUpper x) : xs)

applyTwice f = f . f
applyFourTimes = applyTwice . applyTwice

prop_capitalizeWordIdempotence :: String -> Bool
prop_capitalizeWordIdempotence x = 
 (capitalizeWord x) 
 ==  (applyTwice capitalizeWord . applyFourTimes capitalizeWord $ x)
                    

prop_sortIdempotence :: [Int] -> Bool
prop_sortIdempotence xs =
 (sort xs)
 == (applyTwice sort . applyFourTimes sort $ xs)

--
-- ==================================  make Gen random Generator ======
--

data Fool =
    Fulse
    | Frue
    deriving (Eq, Show)

generateFoolsEqually :: Gen Fool -- equal probability of wither being Fulse or Frue 
generateFoolsEqually = elements [Fulse,Frue]

generateFoolsTwoToThree :: Gen Fool -- 2/3s chance of Fulse, 1/3 chance of Frue.
generateFoolsTwoToThree = elements [Fulse, Fulse, Frue]

-- usage:  sample' generateFools
