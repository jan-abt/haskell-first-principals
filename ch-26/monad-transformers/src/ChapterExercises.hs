module ChapterExercises where


import Control.Monad.Trans.Reader (
    ask,
    runReader,
    runReaderT,
    Reader,
    ReaderT(ReaderT)
 )
import Control.Monad.Identity (Identity, IdentityT(IdentityT))
import Control.Monad.Reader (MonadTrans(lift))
import Control.Monad.Trans.State 
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad (guard)

rDec :: Num a => Reader a a
rDec =
    ReaderT (\env -> return (env - 1))
-- runReader rDec 1      
-- fmap (runReader rDec) [1..10]  

rDec' :: Num a => Reader a a
rDec' =
    ReaderT $ return . subtract 1

-- runReader rDec' 1        
-- fmap (runReader rDec') [1..10]

rDec'' :: Num a => Reader a a
rDec'' =
    ask >>= \env -> return (env - 1)
-- runReader rDec'' 1        
-- fmap (runReader rDec'') [1..10]

rDec''' :: Num a => Reader a a
rDec''' =  subtract 1 <$> ask
-- runReader rDec''' 1        
-- fmap (runReader rDec''') [1..10]    

-- ======================================================================== --   

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT (\env -> return (show env) )

-- runReader rShow 1    
-- fmap (runReader rShow) [1..10]

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

-- runReader rShow' 1    
-- fmap (runReader rShow') [1..10]


rShow'' :: Show a => ReaderT a Identity String
rShow'' =
    ask >>= return . show

-- runReader rShow'' 1    
-- fmap (runReader rShow'') [1..10]

rShow''' :: Show a => ReaderT a Identity String
rShow''' =  show <$> ask

-- runReader rShow''' 1    
-- fmap (runReader rShow''') [1..10]    

-- ======================================================================== --   

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
    ReaderT (\env -> do
        putStrLn $ "Hi: " ++ show env
        return (env+1)
    )
-- runReaderT rPrintAndInc 1
-- mapM (runReaderT rPrintAndInc) [1..10]
-- traverse (runReaderT rPrintAndInc) [1..10]

rPrintAndInc' :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc' =
   ask >>= \env -> 
        lift $ putStrLn ("Hi: " ++ show env) >> 
            pure (env+1)
    
-- runReaderT rPrintAndInc' 1
-- mapM (runReaderT rPrintAndInc') [1..10]
-- traverse (runReaderT rPrintAndInc') [1..10]    

-- ======================================================================== --   

sPrintIncAccum :: (Num a, Show a) => StateT a IO String 
sPrintIncAccum = 
    StateT (\s -> do
        _ <- putStrLn ("Hi: " ++ show s)
        return (show s, s+1)
     )    
-- runStateT sPrintIncAccum 1     
-- mapM (runStateT sPrintIncAccum) [1..5]

sPrintIncAccum' :: (Num a, Show a) => StateT a IO String 
sPrintIncAccum' =  
    get >>= \s -> 
        lift (putStrLn ("Hi: " ++ show s)) >> 
            put (s + 1) >> 
                return (show s) 
        
-- runStateT sPrintIncAccum' 1     
-- mapM (runStateT sPrintIncAccum') [1..5]    

-- ======================================================================== --   

isValidProclamation :: String -> Bool 
isValidProclamation v = '!' `elem` v

maybeLouder :: MaybeT IO String
maybeLouder = do
    v <- lift getLine 
    -- Common uses of guard include conditionally signaling an error in an error monad 
    guard $ isValidProclamation v 
    return v

mkProclamation :: IO () 
mkProclamation = do
    putStrLn "Fac proclamationem!" 
    possibleAnnouncement <- runMaybeT maybeLouder
    case possibleAnnouncement of
        Nothing -> putStrLn "Exclama!"
        Just announcement -> putStrLn ("Bene factum, tu proclamati: " ++ announcement)    