module Lists where

--  LISTS
--
--      1 : 2 : 3 : []
-- 
--  OR
-- 
--      1 : (2 : (3 : []))
-- 
--  OR
-- 
--      : 
--     / \ 
--    1   :
--       / \
--      2   :
--         / \ 
--        3  [ ]
--
-- The list datatype
--
-- data [ ] a = [ ] | a:[a]
--
-- (breakown)
-- data [ ]  a   =  [ ]  |   a : [a]
--      [1] [2] [3] [4] [5]   [6]
--
-- [1] The datatype with the type constructor []
-- [2] takes a single type constructor argument ‘a’
-- [3] at the term level can be constructed via
-- [4] nullary constructor []
-- [5] or it can be constructed by
-- [6] data constructor (:) which is a product of a value of the type a mentioned in the
--     type constructor and a value of type [a], that is, “more list.”
--
--
--  Example: [1, 2, 3]
--
--    `3` is of type `a`.
--    `[2, 3]` is of type `[a]`.
--    `[1, 2, 3]` is created by adding `1` at the beginning of `[2, 3]`, which is essentially expressed as `1 : [2, 3]`.
--
--
-- The concept of the cons operator `(:)` in Haskell.
--
-- 1. Data Constructor:
--    In Haskell, the `:` operator is a data constructor.
--    It's used to create values of a specific data type, which is a list.
--
-- 2. Product of Two Values:
--    The `:` constructor is a product of two values:
--     - The first value is of type `a`.
--     - The second value is of type `[a]`, which is a list that can contain elements of type `a`.
--
-- 3. Infix Operator:
--    The `:` operator is an infix operator.
--    It is positioned between the two arguments it takes, i.e., `a` and `[a]`.
--    For example, you can use it as `x : xs`, where `x` is of type `a`, and `xs` is of type `[a]`.
--
-- 4. Recursion:
--    The important aspect of the `:` operator is that it's recursive.
--    It mentions its own data type `[a]` as one of the members of the product.
--    This is the key to building lists in Haskell.
--
--    When you use `:` to create a list, you're essentially saying that the list is constructed by
--    adding an element `x` of type `a` at the beginning of an existing list `xs` of type `[a]`.
--    So, `xs` can be another list created in the same way.
--
--
--
-- :info Enum
-- type Enum :: * -> Constraint
-- class Enum a where
--    succ :: a -> a
--    pred :: a -> a
--    toEnum :: Int -> a
--    fromEnum :: a -> Int
--    enumFrom :: a -> [a]
--    enumFromThen :: a -> a -> [a]
--    enumFromTo :: a -> a -> [a]
--    enumFromThenTo :: a -> a -> a -> [a]
-- {-# MINIMAL toEnum, fromEnum #-}
--       	-- Defined in ‘GHC.Enum’
-- instance Enum Ordering -- Defined in ‘GHC.Enum’
-- .
-- instance Enum Int -- Defined in ‘GHC.Enum’
-- instance Enum Char -- Defined in ‘GHC.Enum’
-- instance Enum Bool -- Defined in ‘GHC.Enum’
-- .
-- .
-- .

-- RE-IMPLEMENTING SOME FUNCTIONS OF THE ENUM TYPE CLASS 
enumFromToBool :: Bool -> Bool -> [Bool]
enumFromToBool b1 b2 = case b1 `compare` b2 of
                    LT -> b1:b2:[]
                    EQ -> [b1]
                    GT -> []

enumFromToOrdering :: Ordering -> Ordering -> [Ordering]
enumFromToOrdering o1 o2 =
    case o1 `compare` o2 of
       LT -> [o1 .. o2]  
       EQ -> [o1]
       GT -> []

enumFromToInt :: Int -> Int -> [Int]
enumFromToInt i1 i2 =
    case i1 `compare` i2 of
       LT -> [i1..i2]
       EQ -> [i1]
       GT -> []

enumFromToChar :: Char -> Char -> [Char]
enumFromToChar c1 c2 = 
    case c1 `compare` c2 of
       LT -> [c1 .. c2]
       EQ -> [c1]
       GT -> []


sentenceToWords' :: String -> [String] 
sentenceToWords' [] = []
sentenceToWords' (' ':xs) = sentenceToWords' xs 
sentenceToWords' xs =
  (takeWhile (/= ' ') xs):(sentenceToWords' $ dropWhile (/= ' ') xs)


sentenceToWords :: String -> [String]
sentenceToWords sentence = tokenize sentence ' ' []
                 
textToLines :: String -> [String]
textToLines text = tokenize text '\n' []
 
tokenize :: String -> Char -> [String] -> [String] 
tokenize [] sepatator tokens = tokens -- fully parsed, done
tokenize (' ':xs) sepatator tokens = tokenize xs sepatator tokens -- remove leading white space
tokenize content sepatator tokens =
         let token = takeWhile ( /= sepatator) content
             rest = dropWhile ( /= sepatator) content
         in tokenize rest sepatator (tokens ++ [token])
                
