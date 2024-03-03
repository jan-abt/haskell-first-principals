{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import System.Environment (getArgs)
import Debug.Trace (traceIO)
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans

data Config = Config {
    counter :: IORef (M.Map Text Integer),
    prefix :: Text
}

type Scotty = ScottyT (ReaderT Config IO) ()
type ActionHandler = ActionT (ReaderT Config IO) ()

app :: Scotty
app = do
    get "/:key" handleGETAction

handleGETAction :: ActionHandler
handleGETAction =   do
        config <- ask
        paramValue <- pathParam @Text "key"

        let hitCount = getHitCount paramValue config

        -- Lift the hitCount IO action into the Scotty monad
        result <- liftIO $ TL.pack . show <$> hitCount 

        html $ mconcat [ 
                "<h1>Success! Count is: ", 
                    result,
                "</h1>"]

getHitCount:: Text -> Config -> IO Integer
getHitCount ky cnf = 
    let key' = prefix cnf <> "-" <> ky
    in updateMap key' (counter cnf)             
    where
        updateMap :: Text -> IORef (M.Map Text Integer) -> IO Integer
        updateMap k mRef = do 
            m <- readIORef mRef
            case M.lookup k m of
                Just i  -> do
                    let updatedMap = M.insert k (i+1) m
                    traceIO $ show updatedMap
                    traceIO $ "update: "  ++ show (M.toList updatedMap)
                    writeIORef mRef updatedMap
                    pure (i+1)
                Nothing -> do
                    let updatedMap = M.insert k 1 m
                    traceIO $ "insert: " ++ show ( M.toList updatedMap)
                    writeIORef mRef updatedMap
                    pure 1

{-
    on using back-ticks. e.g `runReaderT`
    runReaderT :: ReaderT r m a -> r -> m a

    the usage of back-ticks here is a technique called "sectioning" or "partial application",
    which converts an infix function into a prefix function. 
    This makes it possible to partially apply the function with its SECOND argument.
    
    intStr :: Int -> String -> String
    intStr i s = show i ++ s
    
    let c = (`intStr` "A")
    c 5 
-}
runWithConfig ::Config -> ReaderT Config m a -> m a
runWithConfig cfg = (`runReaderT` cfg)    

-- ghci> :main a-prefix-to-be-stored-in-config
main :: IO ()
main = do
    [arg] <- getArgs
    store     <- newIORef M.empty
    let cfg = Config store (TL.pack arg)

    {-
        scottyT's type 
        Assuming that m is Monad and n is MonadIO
        
        It's general type is             
            Port -> (m Response -> IO Response) -> ScottyT m () -> n () 
        
        Here it is sepecialized to         
                     |------ m -------|                                     |------ m -------|      | n |
            Port -> (ReaderT Config IO Response -> IO Response) -> ScottyT (ReaderT Config IO) () -> IO  ()
    -}
    scottyT 3000 (runWithConfig cfg) app
