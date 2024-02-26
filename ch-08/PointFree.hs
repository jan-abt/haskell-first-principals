module PointFree where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1 

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO () 
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  print (addOnePF 0)
  print ((addOne . addOne) 0) 
  print ((addOnePF . addOne) 0) 
  print ((addOne . addOnePF) 0) 
  print ((addOnePF . addOnePF) 0) 
  print (negate (addOne 0))
  print ((negate . addOne) 0) 
  print ((addOne . addOne . addOne . negate . addOne) 0)

--------------------------------------------------------
--------------------------------------------------------

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = t `div` 10
    where (n,t) = x `divMod` 100


hundredsDigit' :: Integral a => a -> a
hundredsDigit' x = t `div` 100
     where (n,t) = x `divMod` 1000


hundredsDigit'' :: Integral a => a -> a
hundredsDigit'' x = t `div` 100
      where     t = snd $ (flip divMod 1000) x


hundredsDigitComp :: Integral a => a -> a
hundredsDigitComp x  = (flip div 100) . snd $ (flip divMod 1000) $ x

