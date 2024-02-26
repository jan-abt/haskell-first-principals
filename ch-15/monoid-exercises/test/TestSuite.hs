module Main where

import Lib
import Test.Hspec
import Test.QuickCheck
import Data.Monoid

type IdentityAssocSum = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool
type IdentitySum =  Identity (Sum Int) -> Bool  

type IdentityAssocProduct = Identity (Product Int) -> Identity (Product Int) -> Identity (Product Int) -> Bool
type IdentityProduct =  Identity (Product Int) -> Bool  

type IdentityAssocString = Identity String -> Identity String -> Identity String -> Bool
type IdentityString =  Identity String -> Bool  

type TwoAssoc = Two (Sum Int) ConjunctiveBool -> Two (Sum Int) ConjunctiveBool  -> Two (Sum Int) ConjunctiveBool  -> Bool
type TwoIdentity =  Two (Sum Int) ConjunctiveBool -> Bool  

type TwoAssoc' = Two (Sum Int) DisjunctiveBool -> Two (Sum Int) DisjunctiveBool  -> Two (Sum Int) DisjunctiveBool  -> Bool
type TwoIdentity' =  Two (Sum Int) DisjunctiveBool -> Bool  
  
type MemAssoc = Sum Int -> Mem (Sum Int) (Sum Int) -> Mem (Sum Int) (Sum Int)-> Mem (Sum Int) (Sum Int)-> Bool
type MemIdentity =  Sum Int -> Mem (Sum Int) (Sum Int)-> Bool  

type ComposeFAssoc = Sum Int -> ComposeF (Sum Int) -> ComposeF (Sum Int) -> ComposeF (Sum Int) -> Bool
type ComposeFIdentity =  Sum Int -> ComposeF (Sum Int) -> Bool  

type CombineFAssoc = Sum Int -> CombineF (Sum Int) (Sum Int) -> CombineF (Sum Int) (Sum Int)-> CombineF (Sum Int) (Sum Int)-> Bool
type CombineFIdentity =  Sum Int -> CombineF (Sum Int) (Sum Int)-> Bool  
    
-- ============= QUICK CHECK PROPERTIES  ======================
prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
prop_monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)  

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a 

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a

-- quickCheck (prop_memAssoc :: MemAssoc )
prop_memAssoc :: (Eq a, Eq b, Monoid a, Monoid b) => a -> Mem a b -> Mem a b -> Mem a b -> Bool
prop_memAssoc v a b c = (runMem (a <> (b <> c)) $ v) == (runMem ((a <> b) <> c) $ v)    

-- quickCheck (prop_memLeftIdentity :: MemIdentity )
prop_memLeftIdentity ::(Eq a, Eq b, Monoid a, Monoid b) => a -> Mem a b -> Bool
prop_memLeftIdentity v a = (runMem (mempty <> a) $ v) == (runMem a $ v)

-- quickCheck (prop_memRightIdentity :: MemIdentity )
prop_memRightIdentity :: (Eq a, Eq b, Monoid a, Monoid b) => a -> Mem a b -> Bool
prop_memRightIdentity v a = (runMem (a <> mempty) $ v) == (runMem a $ v) 

-- quickCheck (prop_composeFAssoc :: ComposeFAssoc )
prop_composeFAssoc :: (Monoid m, Eq m) => m -> ComposeF m -> ComposeF m -> ComposeF m -> Bool
prop_composeFAssoc v a b c = (composeAll (a <> (b <> c)) $ v) == (composeAll ((a <> b) <> c) $ v)    

-- quickCheck (prop_composeFLeftIdentity :: ComposeFIdentity )
prop_composeFLeftIdentity :: (Eq m, Monoid m) => m -> ComposeF m -> Bool
prop_composeFLeftIdentity v a = (composeAll (mempty <> a) $ v) == (composeAll a $ v)

-- quickCheck (prop_composeFRightIdentity :: ComposeFIdentity )
prop_composeFRightIdentity :: (Eq m, Monoid m) => m -> ComposeF m -> Bool
prop_composeFRightIdentity v a = (composeAll (a <> mempty) $ v) == (composeAll a $ v)

-- quickCheck (prop_combineFAssoc :: CombineFAssoc )
prop_combineFAssoc :: (Eq a, Monoid a, Eq b, Monoid b) => a -> CombineF a b -> CombineF a b -> CombineF a b -> Bool
prop_combineFAssoc v a b c = (applyToAll (a <> (b <> c)) $ v) == (applyToAll ((a <> b) <> c) $ v)    

-- quickCheck (prop_combineFLeftIdentity :: CombineFIdentity )
prop_combineFLeftIdentity :: (Eq a, Monoid a, Eq b, Monoid b) => a -> CombineF a b -> Bool
prop_combineFLeftIdentity v a = (applyToAll (mempty <> a) $ v) == (applyToAll a $ v)

-- quickCheck (prop_combineFRightIdentity :: CombineFIdentity )
prop_combineFRightIdentity :: (Eq a, Monoid a, Eq b, Monoid b) => a -> CombineF a b -> Bool
prop_combineFRightIdentity v a = (applyToAll (a <> mempty) $ v) == (applyToAll a $ v)  


main :: IO () 
main = do
  unitTests
  quickCheckTests

unitTests :: IO () 
unitTests = hspec $ do 
  
  describe "unit tests" $ do 

    it "Some (Sum 1) `mappend` Some (Sum 1)" $ do
      let n = (Some (Sum 1) `mappend` Some (Sum 1))
      getSum <$> n `shouldBe` (Some (2::Int))

    it "Some (Product 2) `mappend` Some (Product 3)" $ do
      let n = (Some (Product 2) `mappend` Some (Product 3))
      getProduct <$> n `shouldBe` (Some (6::Int))

    it "Some (Sum 1) `mappend` None" $ do
      let n = Some (Sum 1) `mappend` None
      getSum <$> n `shouldBe` (Some (1::Int))

    it "None `mappend` Some (Sum 1)" $ do
      let n = None `mappend` Some (Sum 1)
      getSum <$> n `shouldBe` (Some (1::Int))

    it "(Some J `mappend` ((Some a) `mappend` (Some n))" $ do
      let result = (Some "J") `mappend` ((Some "a") `mappend` (Some "n"))
      result `shouldBe` (Some "Jan")

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
    
    it "associativity rule check of monoids with arbitrary TrivialAssoc input" $ do
      quickCheck (prop_monoidAssoc :: TrivialAssoc)
    it "left identity rule check with arbitrary TrivialIdentity input" $ do
      quickCheck (prop_monoidLeftIdentity :: TrivialIdentity)
    it "right identity rule check with arbitrary TrivialIdentity input" $ do
      quickCheck (prop_monoidRightIdentity :: TrivialIdentity)
      
    it "associativity rule check of monoids with arbitrary IdentityAssocSum input" $ do
      quickCheck (prop_monoidAssoc :: IdentityAssocSum)
    it "left identity rule check with arbitrary IdentitySum input" $ do
      quickCheck (prop_monoidLeftIdentity :: IdentitySum)
    it "right identity rule check with arbitrary IdentitySum input" $ do
      quickCheck (prop_monoidRightIdentity :: IdentitySum)
      
    it "associativity rule check of monoids with arbitrary IdentityAssocProduct input" $ do
      quickCheck (prop_monoidAssoc :: IdentityAssocProduct)
    it "left identity rule check with arbitrary IdentityProduct input" $ do
      quickCheck (prop_monoidLeftIdentity :: IdentityProduct)
    it "right identity rule check with arbitrary IdentityProduct input" $ do
      quickCheck (prop_monoidRightIdentity :: IdentityProduct)
      
    it "associativity rule check of monoids with arbitrary IdentityAssocString input" $ do
      quickCheck (prop_monoidAssoc :: IdentityAssocString)
    it "left identity rule check with arbitrary IdentityString input" $ do
      quickCheck (prop_monoidLeftIdentity :: IdentityString)
    it "right identity rule check with arbitrary IdentityString input" $ do
      quickCheck (prop_monoidRightIdentity :: IdentityString)
      
    it "associativity rule check of monoids with arbitrary TwoAssoc input" $ do
      quickCheck (prop_monoidAssoc :: TwoAssoc)
    it "left identity rule check with arbitrary TwoIdentity input" $ do
      quickCheck (prop_monoidLeftIdentity :: TwoIdentity)
    it "right identity rule check with arbitrary TwoIdentity input" $ do
      quickCheck (prop_monoidRightIdentity :: TwoIdentity)
      
    it "associativity rule check of monoids with arbitrary TwoAssoc' input" $ do
      quickCheck (prop_monoidAssoc :: TwoAssoc')
    it "left identity rule check with arbitrary TwoIdentity' input" $ do
      quickCheck (prop_monoidLeftIdentity :: TwoIdentity')
    it "right identity rule check with arbitrary TwoIdentity' input" $ do
      quickCheck (prop_monoidRightIdentity :: TwoIdentity')
       
    it "associativity rule check of monoids with arbitrary CombineFAssoc input" $ do
      quickCheck (prop_combineFAssoc :: CombineFAssoc )
    it "left identity rule check with arbitrary CombineFIdentity input" $ do
      quickCheck (prop_combineFLeftIdentity :: CombineFIdentity )
    it "right identity rule check with arbitrary CombineFIdentity input" $ do
      quickCheck (prop_combineFRightIdentity :: CombineFIdentity )

    it "associativity rule check of monoids with arbitrary ComposeFAssoc input" $ do
      quickCheck (prop_composeFAssoc :: ComposeFAssoc )
    it "left identity rule check with arbitrary ComposeFIdentity input" $ do
      quickCheck (prop_composeFLeftIdentity :: ComposeFIdentity )
    it "right identity rule check with arbitrary ComposeFIdentity input" $ do
      quickCheck (prop_composeFRightIdentity :: ComposeFIdentity )

    it "associativity rule check of monoids with arbitrary MemAssoc input" $ do
      quickCheck (prop_memAssoc :: MemAssoc )
    it "left identity rule check with arbitrary MemIdentity input" $ do
      quickCheck (prop_memLeftIdentity :: MemIdentity )
    it "right identity rule check with arbitrary MemIdentity input" $ do
      quickCheck (prop_memRightIdentity :: MemIdentity )
     



  