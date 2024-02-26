module ParameterizedDataType  where

-- writing an instance of the Eq type class where the instance data type 
-- has a polymorphic parameter, here "a" of "Indentity a"
data Identity a = Identity a
-- we can enforce a typeclass constraint to let it be known that the polimorphic 
-- parameter of "Identity", "a" needs to have an instance of Eq
instance (Eq a) => Eq (Identity a) where
 (==) (Identity i) (Identity i') = i == i'

