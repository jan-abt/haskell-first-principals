
module Exercises  where

foldBoolPattern :: a -> a -> Bool -> a 
foldBoolPattern x y True = x 
foldBoolPattern x y False = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b = case b of
                        True -> x
                        False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b | b == True = x
                    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree  = read . show 

roundTripPointFree' :: (Show a, Read b) => a -> b
roundTripPointFree'  = read . show  
-- usage: roundTripPointFree' 4::Int
--        roundTripPointFree' "Jan"::String
