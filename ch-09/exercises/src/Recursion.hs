module Recursion where

-- NOT RECURSIVE
increaseThreeTimesByOne :: Num a => a -> a 
increaseThreeTimesByOne n = inc . inc . inc $ n
  where inc = (+1)

-- RECURSIVE: BUILD A NESTED EXPRESSION CHAIN, UNTIL THE EDGE CONDITION IS MET, THEN EVALUATE
applyPlus1NTimes :: (Eq a, Num a) => a -> a -> a 
applyPlus1NTimes 0 x = x
applyPlus1NTimes n x = 1 + (applyPlus1NTimes (n - 1) x)

-- EXPANSION EXAMPLE: DO `+ 1` 3 TIMES, STARTING AT 5  
-- f 3 5 = 1 + ((3-1) 5)
-- f 2 5 = 1 + (1 + (2-1) 5)   
-- f 1 5 = 1 + (1 + (1 + ((1-1) 5)))
-- f 0 5 = 1 + (1 + (1 + 5)) <- this is the completed expression to be evaluated

-- ABSTRACTING OUT THE RECURSIVE PART: 

-- Tail resursive !
-- Reason: the recursive call to "applyFNTimes" is the last/innermost operation before returning the result. 
-- There are no pending operations to perform after the recursive cal
applyFNTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyFNTimes' 0 f n = n
applyFNTimes' c f n = f (applyFNTimes' (c-1) f n)

--  EXPANSION EXAMPLE, apply `+ 1` 5 TIMES, STARTING AT 5 
--  applyFNTimes 5 (+) 5
--                  = 1 + applyFNTimes ( (5-1) f 5)
--                  = 1 + (1 + applyFNTimes ( (4-1) f 5) )
--                  = 1 + (1 + (1 + applyFNTimes ( (3-1) f 5) ) )
--                  = 1 + (1 + (1 + (1 + applyFNTimes ( (2-1) f 5) ) ) )
--                  = 1 + (1 + (1 + (1 + (1 + applyFNTimes ( (1-1) f 5) ) ) ) )
--                  = 1 + (1 + (1 + (1 + (1 + (5) ) ) ) )

-- same as previous, applyFNTimes', but implemented in function composition style 
applyFNTimes'' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyFNTimes'' 0 f n = n
applyFNTimes'' c f n = f . applyFNTimes'' (c-1) f $ n

-- Not tail-recursive !
-- Reason: the last/innermost recursive call to "applyFNTimes" is followed by an operation, "f n" before returning the result. 
applyFNTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b 
applyFNTimes 0 f n = n 
applyFNTimes c f n = applyFNTimes (c-1) f (f n)

-- INFINITE SERIES: an infinitely growing fibonacci sequence
fibonacciSeq :: [Integer]
fibonacciSeq = step 0 [1]
  where step current fibSeq@(previous:_) = 
               let nextFib = current + previous 
                   newSeq  = (current:fibSeq)
               in nextFib:(step nextFib newSeq)

fibonacciSeq' :: [Integer]
fibonacciSeq' = 1 : scanl (+) 1 fibonacciSeq'


-- returns fiboncacci number at index  
nthFibonacciNum :: Integer -> Integer
nthFibonacciNum 0 = 1
nthFibonacciNum 1 = 1
nthFibonacciNum n = nthFibonacciNum (n - 2) + nthFibonacciNum (n - 1)

nthFibonacciNum' :: Integer -> Integer
nthFibonacciNum' n = fib n [1,0]
 where fib 0 [1,0] = 1
       fib 1 [1,0] = 1
       fib 0 xs = head xs
       fib i seq@(x:y:ys) = fib (i-1) ((x+y):seq)

-- divison
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)
 
-- example: 25 `dividedBy` 4  = (6, 1) 
--  25 dividedBy 4 == 25 - 4, 21 (1st)
--                       - 4, 17 (2nd)
--                       - 4, 13 (3rd)
--                       - 4,  9 (4th)
--                       - 4,  5 (5th)
--                       - 4,  1 (6th)
--  stop at 1 because it's less than 4

-- a recursive implementation of `sum`
sum'::(Eq a, Num a) => a -> a
sum' n = s n n
  where s 0 acc = acc
        s n acc = s (n-1) (acc+(n-1))

-- left shift operation (base value m is "powered" to the value of n) 
shiftL::(Eq a, Num a) => a -> a -> a 
shiftL x n = shift n x 
  where shift 0 acc = 1
        shift 1 acc = acc
        shift n acc = shift (n-1) (x * acc)

-- example: "shiftL" operations using base 2
-- shiftL 2 0  =   1
-- shiftL 2 1  =   2
-- shiftL 2 2  =   4
-- shiftL 2 3  =   8
-- shiftL 2 4  =  16
-- shiftL 2 5  =  32
-- shiftL 2 6  =  64
-- shiftL 2 7  = 128
      


