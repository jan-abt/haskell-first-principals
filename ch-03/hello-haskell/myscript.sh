#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

{- project: 
with-compiler: ghc-9.6.3
-}

import Data.List.Split (chunksOf)

main :: IO ()
main = do 
 putStrLn "Enter some text:"
 getLine >>= print . chunksOf 3