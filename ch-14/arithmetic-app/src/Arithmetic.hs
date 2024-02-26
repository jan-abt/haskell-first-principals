-- Arithmetic.hs
module Arithmetic where

import Test.Hspec
import Test.QuickCheck
import ArbitraryInstances

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = continueWith num denom 0
 where 
  continueWith n d count
   | n < d = (count, n)
   | otherwise = continueWith (n - d) d (count + 1)

sumOf ::  (Eq a, Num a) => [a]-> a 
sumOf [] = 0
sumOf (n:ns) = n + sumOf (ns)

recursiveSummation :: Int -> Int
recursiveSummation 0 = 0
recursiveSummation n = n + recursiveSummation (n-1)

main :: IO () 
main = hspec $ do
 
 describe "Addition" $ do
  it "1 + 1 is greater than 1" $ do
   (1 + 1) > (1::Int) `shouldBe` True
  it "x + 1 is always greater than x" $ do
   property $ \x -> x + 1 > (x :: Int)
  it "2 + 2 is equal to 4" $ do 
   2 + 2 `shouldBe` (4::Int)

 describe "Division" $ do
  it "15 divided by 3 is 5" $ do
   dividedBy 15 3 `shouldBe` (5::Int, 0)
  it "22 divided by 5 is 4 remainder 2" $ do
   dividedBy 22 5 `shouldBe` (4::Int, 2)
  it "5 divided by 3 is 1 remainder 2" $ do
   dividedBy 5 3 `shouldBe` (1::Int, 2)
 
 describe "Sum of List" $ do
  it "sumOf [] is 0" $ do
   sumOf [] `shouldBe` (0::Int)
  it "sumOf [1,2,3,4,5,6,7,8,9,10] is 55" $ do
   sumOf [1,2,3,4,5,6,7,8,9,10] `shouldBe` (55::Int)
 
 describe "Recursive Summation" $ do
  it "recursiveSummation 5 is 15" $ do
   recursiveSummation 5 `shouldBe` (15::Int)

--
-- =========================== Quick Check without HSpec ==============
--
-- ghci> :t quickCheck 
-- quickCheck :: Testable prop => prop -> IO ()
--
-- ghci> :t property
-- property :: Testable prop => prop -> Property


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

additionGreaterQC :: IO ()
additionGreaterQC = quickCheck prop_additionGreater

-- The Person data type has defined an instance of Arbitrary,seen MyTypeQuickChec.hs
-- it tells it how to contruct samples of that data type
prop_PersonOlder :: Person -> Bool
prop_PersonOlder (Person _ age) = age >=  0

personOlderQC :: IO ()
personOlderQC = quickCheck prop_PersonOlder



