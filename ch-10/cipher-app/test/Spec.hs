-- tests/Tests.hs
module Main  where

import Test.Hspec
import Test.QuickCheck
import Lib (Direction(..), encode, decode, raster)

main :: IO ()
main = hspec $ do
 describe "\n***\n*** unit tests *********\n***" $ do
  it "\n--- template unit test ---" $ do
    (0 :: Int)  `shouldBe` 0 --cast to Int otherewise "shouldBe" will default to Integer and generate a warning message
 describe "\n***\n*** property tests *********\n***" $ do
   it "\n--- \"my data\" === decode . encode $ \"my data\" ---" $ do
 -- if you want to print generated data to the outputstream, use: 
 -- quickCheck $ verbose $ ...
    quickCheck $ forAll makePwdAndText prop_encode_decode

makePwdAndText :: Gen (String, String)
makePwdAndText = 
 let pwd   =  vectorOf 5 (elements $ raster LTR)
     given = vectorOf 30 (elements $ raster LTR)
 in (,) <$> pwd <*> given

-- Your property test
prop_encode_decode :: (String , String) -> Property
prop_encode_decode given =
 snd given === ( decode (fst given) . encode (fst given) $ (snd given) )
