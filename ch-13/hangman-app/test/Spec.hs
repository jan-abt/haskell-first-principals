-- tests/Tests.hs
module Main where


import Test.Hspec
import Test.QuickCheck
import  Lib (Puzzle(..), fillInCharacter, handleGuess)

-- needed to be able to provide random Puzzle data
instance Arbitrary Puzzle
 where
  arbitrary = do
    theWord  <- vectorOf 3 (elements (['a'..'c'])) 
    return ( Lib.Puzzle theWord (replicate 3 Nothing) "")

-- ghci> fillInCharacter (Puzzle "ajay" (replicate 4 Nothing) "") 'a'
-- a _ a _ Guessed so far: a
prop_fillInCharacter :: Puzzle -> Char -> Property
prop_fillInCharacter puzzle@(Puzzle theWord _ _) char = 
 char `elem` ['a'..'c'] ==>
  char `elem` (show (fillInCharacter puzzle char) )

main :: IO ()
main = hspec $ do

 describe "\n***\n*** unit  tests *********\n***" $ do
  it "\n--- already guessed ---" $ do
    let original =  Puzzle "ajay" [Just 'a', Nothing, Just 'a', Nothing] "a"
    updated <- handleGuess original 'a'
    updated `shouldBe` original
  it "\n--- guessed correctly ---" $ do
     let original =  Puzzle "ajay" [Just 'a', Nothing, Just 'a', Nothing] "a"
     let expected = Puzzle "ajay" [Just 'a', Nothing, Just 'a', Just 'y'] "ya"
     updated <- handleGuess original 'y'
     updated `shouldBe` expected   
  it "\n--- fillInCharacter ---" $ do
   (show $ fillInCharacter (Puzzle "abc" (replicate 3 Nothing) "") 'a') `shouldBe` "a _ _ Guessed so far: a" 
      
 describe "\n***\n*** spec tests *********\n***" $ do
  it "\n--- should guess some characters ---" $ do
   -- quickCheck $ verbose prop_fillInCharacter   
   quickCheck  prop_fillInCharacter   
