-- ch-07/Numberish.hs


-- class definitions are comprised of a set of operations and values all instances will provide. 
-- They don’t define any terms or actual code implementations which we can compile and execute, only types.
-- The instances are unique pairings of 
--      a. the type the instance is for and
--      b. the class the instance is being defined for 


newtype Age = Age Integer deriving (Eq, Show)
newtype Year = Year Integer deriving (Eq, Show)

class Numberish a where 
  fromNumber :: Integer -> a 
  toNumber :: a -> Integer 
  defaultNumber :: a -- don't use defaults like this
  -- Don't use typeclasses to define default values.
  -- We would have to resort to type assertion to dispatch, or specify, 
  -- what typeclass instance we want to get our defaultNumber from.

instance Numberish Age where 
  fromNumber n = Age n 
  toNumber (Age n) = n 
  defaultNumber = Age 65 -- don't use defaults like this
  -- would need type assertion to get our defaultNumber ::Age

instance Numberish Year where 
  fromNumber n = Year n 
  toNumber (Year n) = n 
  defaultNumber = Year 1988 -- don't use defaults like this
  -- would need type assertion to get our defaultNumber ::Year
    
sumNumberish :: Numberish a => a -> a -> a 
sumNumberish a a' = fromNumber summed
 where integerOfA      = toNumber a
       integerOfAPrime = toNumber a'
       summed          = integerOfA + integerOfAPrime

