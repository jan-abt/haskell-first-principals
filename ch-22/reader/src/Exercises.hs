module
    Exercises
        where


import Control.Monad.Reader
import           Data.Map (Map)
import qualified Data.Map as Map
   

type Bindings = Map String Int

validateBindingsCount :: IO ()
validateBindingsCount = do
    let bindings = loadBindings
    putStrLn $ "Is count correct for bindings: \n" ++ (show $ Map.assocs bindings)
    putStrLn $ show (isCountCorrect bindings)

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader compareEntryWithCount bindings

-- The Reader monad, which implements the check.
compareEntryWithCount :: Reader Bindings Bool
compareEntryWithCount = do
    count <- asks (lookupBinding "count")
    bindings <- ask
    return (count == (Map.size bindings))

-- The selector function to use with 'asks'.
-- Returns value of the variable with specified name.
lookupBinding :: String -> Bindings -> Int
lookupBinding name bindings = maybe 0 id (Map.lookup name bindings)

loadBindings :: Bindings
loadBindings = Map.fromList [("count", 6), ("a", 1), ("b", 2),("c", 3),("d", 4),("e", 5)]


