module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.QuickCheck ( quickCheck )
import Data.Monoid ( Product(Product, getProduct), Sum(..))
import Lib ( Option(Some, None) ) 

 -- ============= QUICKCHECK PROPERTIES 
prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  unitTests
  quickCheckTests

unitTests :: IO ()
unitTests = hspec $ do

  describe "unit tests" $ do

    it "Some (Sum 1) `mappend` Some (Sum 1)" $ do
      let n = Some (Sum 1) `mappend` Some (Sum 1)
      getSum <$> n `shouldBe` Some (2::Int)

    it "Some (Product 2) `mappend` Some (Product 3)" $ do
      let n = Some (Product 2) `mappend` Some (Product 3)
      getProduct <$> n `shouldBe` Some (6::Int)

    it "Some (Sum 1) `mappend` None" $ do
      let n = Some (Sum 1) `mappend` None
      getSum <$> n `shouldBe` Some (1::Int)

    it "None `mappend` Some (Sum 1)" $ do
      let n = None `mappend` Some (Sum 1)
      getSum <$> n `shouldBe` Some (1::Int)

    it "(Some J `mappend` ((Some a) `mappend` (Some n))" $ do
      let result = Some "J" `mappend` (Some "a" `mappend` Some "n")
      result `shouldBe` Some "Jan"

quickCheckTests :: IO ()
quickCheckTests = hspec $ do

  describe "quick check tests" $ do

    it "associativity rule check of monoids with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidAssoc :: Sum Int -> Sum Int -> Sum Int -> Bool)
    it "associativity rule check of monoids with arbitrary String input" $ do
      quickCheck (prop_monoidAssoc :: String -> String -> String -> Bool)
    it "left identity rule check with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidLeftIdentity :: Sum Int -> Bool)
    it "right identity rule check with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidRightIdentity :: Sum Int -> Bool)

