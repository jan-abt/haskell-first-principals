module Doodles where

doodles :: IO () 
doodles = do
    what <- life'sMeaning
    putStr ("It is ")
    print( what )
    return ()

life'sMeaning :: IO Integer
life'sMeaning = 
    humbleBeginnings >>= stormAndStress >>= enlightenment

 where 
    humbleBeginnings :: IO Integer
    humbleBeginnings = readIO "0" 

    stormAndStress :: Integer -> IO Integer
    stormAndStress i = let storm = readIO $ show i :: IO Integer
                           stress = (\x -> readIO ( "2" ++ show x )   )
                       in storm >>= stress

    enlightenment :: Integer -> IO Integer                  
    enlightenment = (\i -> readIO $ show (2 * i + 2))
   
