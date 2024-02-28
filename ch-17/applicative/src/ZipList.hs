module 
    ZipList 
        where
        
import Control.Applicative  hiding (ZipList, getZipList)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

----------------------------------------------------------------------
-- OPTION I : Implement our own version of ZipList
----------------------------------------------------------------------
newtype ZipList a = 
    ZipList [a] 
    deriving (Ord, Eq, Show)    

instance Semigroup a => Semigroup (ZipList a) 
    where
        (<>) ls ls' =  (<>) <$> ls <*> ls'
    --  (<>) ls ls' = liftA2 (<>) ls ls'
        
instance Functor ZipList 
    where
        fmap f (ZipList a) = ZipList (map f a)

instance (Eq a, Monoid a) => Monoid (ZipList a) 
    where 
        -- using "mempty :: a" here to clearly assert
        -- that we're using the "mempty" of the underlying monoid type "a".
        mempty = pure (mempty :: a)
        mappend ls ls' = (<>) ls ls'
        mconcat ls = foldr mappend mempty $ ls
    
instance Applicative ZipList 
    where
       -- cannot do applicative test against infinite lists. 
       -- For test, one could temprarily change pure to generate a finite list.
       -- quickBatch $ applicative (ZipList [(1,2,3)])
       -- pure a = ZipList (take 100 $ repeat a) 
        pure a = ZipList (repeat a) 
        (<*>) (ZipList fs) (ZipList vs) = merge fs vs []
            where
                merge [] _ ac = ZipList ac
                merge  _ [] ac = ZipList ac
                merge (f:fs) (v:vs) ac = merge fs vs (ac++[f v])
        liftA2 f (ZipList ls) (ZipList ls') = ZipList (zipWith f ls ls')

-- FOR QUICK CHECK DATA GENERATION
instance Arbitrary a => Arbitrary (ZipList a)
    where
        arbitrary = ZipList <$> arbitrary
 
instance Eq a => EqProp (ZipList a) 
    where --(=-=) = eq 
        xs =-= ys = xs' `eq` ys'
            where xs' = let (ZipList l) = xs 
                        in keep 100 l
                  ys' = let (ZipList l) = ys 
                        in keep 100 l

keep :: Int -> [a] -> [a]
keep 0 _ = []
keep n [] = []
keep n (l:ls) = l : (keep (n-1) ls)    

-- sample usage:    
-- quickBatch $ monoid (pure 0 :: ZipList (Sum Int))       
-- ZipList [(+1), (*2)] <*> ZipList [1,2]  
-- ZipList [(+), (*)] <*> ZipList [1,2]  <*> ZipList [1,2]
-- pure (+) <*> ZipList [2] <*> ZipList [2] 

----------------------------------------------------------------------
-- OPTION II : Use the standart Control.Applicative.ZipList
--
-- With this approach, to test our Monoid, we first need to write a wrapper around ZipList. 
-- This is because the latter's instance of Applicative utilizes an infinite list in its implementation of "pure". 
-- This in turn, causes Quickchek to produce a stack overflow when generating test data. 
-- Using the wrapper we can write our own instances of EqProp and Arbitrary, 
-- instead of the one provided by Quickcheck, and control the length of ZipList test data ourselves 
----------------------------------------------------------------------

-- newtype Wrapper a = 
--     Wrapper (ZipList a) 
--     deriving (Eq, Show)

-- instance Semigroup a => Semigroup (Wrapper a) 
--     where
--         (Wrapper zl) <> (Wrapper zl') =  (Wrapper $ fmap (<>) zl <*> zl')
 
-- instance (Monoid a) => Monoid (Wrapper a) 
--     where
--         mempty = Wrapper (pure mempty)
--         mappend zl zl' = (<>) zl zl'
--         mconcat = foldr mappend mempty

-- FOR QUICK CHECK DATA GENERATION
-- instance Arbitrary a => Arbitrary (Wrapper a)
--     where
--         arbitrary = Wrapper <$> arbitrary

-- instance Eq a => EqProp (Wrapper a) 
--     where
--         (=-=) (Wrapper zl) (Wrapper zl') =
--             property $ take 100 (getZipList zl) == take 100 (getZipList zl')


-- quickBatch $ monoid (Wrapper (ZipList [1 :: Sum Int]))

