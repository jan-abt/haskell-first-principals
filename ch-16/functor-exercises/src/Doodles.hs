module Doodles where

 
meaningOfEverything :: IO Integer
meaningOfEverything = 
    let ioi = readIO "1" :: IO Integer
        changed = fmap (\s -> read ("2"++ s) :: Integer) (fmap show ioi)
    in fmap (*2) changed

doodles :: IO () 
doodles = do
    let a = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
    print (a)         
    let b = fmap (fmap (++ "lol"))  (Just ["Hi,", "Hello"])
    print (b)         
    let c = ((return '1' ++) . show) <$> ((\x -> [x, 1..3]) $ 0)
    print (c)         
    let d = ((return '1' ++) . show) <$> (\x -> [x, 1..3]) $ 0
    print (d)         
    value <- meaningOfEverything
    print (value)
    return ()
   