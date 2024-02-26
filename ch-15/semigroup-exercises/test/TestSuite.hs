module Main where

import Lib
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO () 
main = hspec $ do

 -- ============== UNIT TESTS
 describe "unit tests" $ do 

  it "Some (Sum 1) `mappend` Some (Sum 1)" $ do
   let n = (Some (Sum 1) `mappend` Some (Sum 1))
   getSum <$> n `shouldBe` (Some (2::Int))
  it "Identity \"Jan\" <> Identity \" \" <> Identity \"Abt\"" $ do
   let n = (Identity "Jan") <> (Identity " ") <> (Identity "Abt")
   n `shouldBe` (Identity "Jan Abt")
 
 -- ============== QUICKCHECK TESTS
 describe "quick check" $ do 
  it "associativity rule check of monoids with arbitrary Sum Int input" $ do
   quickCheck (prop_monoidAssoc :: Sum Int -> Sum Int -> Sum Int -> Bool)
  it "associativity rule check of monoids with arbitrary String input" $ do
   quickCheck (prop_monoidAssoc :: String -> String -> String -> Bool)
  it "left identity rule check with arbitrary Sum Int input" $ do
   quickCheck (prop_monoidLeftIdentity :: Sum Int -> Bool)
  it "right identity rule check with arbitrary Sum Int input" $ do
   quickCheck (prop_monoidRightIdentity :: Sum Int -> Bool)
  it "associativity rule check of semi groups with arbitrary Trivial input" $ do
   quickCheck (prop_semiGroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  it "associativity rule check of semi groups with arbitrary Identity String input" $ do
   quickCheck (prop_semiGroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  it "associativity rule check of semi groups with arbitrary TwoTypes input" $ do
   quickCheck (prop_semiGroupAssoc :: TwoTypes)
  it "associativity rule check of semi groups with arbitrary ThreeTypes input" $ do
   quickCheck (prop_semiGroupAssoc :: ThreeTypes)
  it "associativity rule check of semi groups with arbitrary ThreeTypes input" $ do
   quickCheck (prop_semiGroupAssoc :: FourTypes)
  it "associativity rule check of semi groups with arbitrary BoolConj input" $ do
   quickCheck (prop_semiGroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  it "associativity rule check of semi groups with arbitrary BoolDisj input" $ do
   quickCheck (prop_semiGroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  it "associativity rule check of semi groups with arbitrary OrType input" $ do
   quickCheck (prop_semiGroupAssoc :: OrType )
  it "associativity rule check of semi groups with arbitrary ValidationType input" $ do
   quickCheck (prop_semiGroupAssoc :: ValidationType )
 
   
--  -- ============== PROPERTIES
prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
prop_monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)  

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a 

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a

prop_semiGroupAssoc :: (Eq s, Semigroup s) => s -> s -> s -> Bool 
prop_semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)  
