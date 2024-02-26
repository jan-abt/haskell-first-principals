#!/usr/bin/env bash

# example:
# ➜  ch-17 ../cabal-seed-projects/cabal-seed-project.sh monad-exercises 17

function main {

cat > ch-$chapter.code-workspace <<EOF
{
	"folders": [     			
		{
			"path": "$project"
		}
	],
	"settings": {}
}
EOF

  
mkdir $project;

chmod 777 $project;

cd $project;

### us it to build initial files and dirs, replace it later
cabal init --non-interactive \
            --license MIT --is-executable --package-name "$project" \
            --author=ja-cha --email=jan_abt@yahoo.com \
            --language=GHC2021 --synopsis "My Project" \
            --version=0.1.0.0 --cabal-version=3.0 \
            --libandexe --no-comments 
        
rm app/Main.hs
rm src/MyLib.hs
rm CHANGELOG.md

mkdir test

cat > CHANGELOG.md <<EOF
# Revision history for: $project

## 0.1.0.0 -- $(date +"%Y-%m-%d")

* First version. Released on an unsuspecting world.

EOF


cat > README.md <<EOF
# README for: $project

## V. 0.1.0.0 -- $(date +"%Y-%m-%d")

#### Useful Cabal commands
* \`cabal clean\`
* \`cabal build\`
* \`cabal test\`
* \`cabal run\`

#### During Development
* \`cabal repl\`<br>
    _A) tests_
	\`\`\`
		ghci> :l test/TestSuite
		ghci> main
	\`\`\`
    _B) app_
	\`\`\`
		ghci> :l app/Main
		ghci> main
	\`\`\`
	_C) src_
	\`\`\`
		ghci> :l src/Doodles
		ghci> life'sMeaning
	\`\`\`
EOF

  
cat > app/Main.hs <<EOF
module 
	Main 
		where

import qualified Lib (sayHi)

main :: IO ()
main = do
  putStrLn "Hello Haskell!"
  Lib.sayHi
EOF

cat > src/Lib.hs <<EOF
module 
	Lib (Option (..), sayHi) 
		where

sayHi :: IO ()
sayHi = putStrLn "Hi from Lib!"


data Option m =
 None
 | Some m
 deriving (Eq, Show)

-- class constraint on 'm' indicates that it also must be a Semigroup, e.g. it understands (<>)
instance Semigroup m => Semigroup (Option m)
 where
  (<>) :: Semigroup m => Option m -> Option m -> Option m
  (<>) None m =  m
  (<>) m None =  m
  (<>) (Some m) (Some m')  = Some ( m <> m')

-- class constraint on 'm' indicates that it also must be a Semigroup, e.g. it understands (<>)
instance Semigroup m => Monoid (Option m)
 where
  mempty :: Semigroup m => Option m
  mempty = None
  
  mappend :: Semigroup m => Option m -> Option m -> Option m
  mappend = (<>)

-- fmap, <$>
instance Functor Option
  where
    fmap :: (a -> b) -> Option a -> Option b
    fmap f (Some a) =  Some (f a)
    fmap _ None =  None


instance Applicative Option
    where
        pure :: a -> Option a
        pure  =  Some

        (<*>) :: Option (a -> b) -> Option a -> Option b
        Some f <*> g = fmap f g
        None  <*> _  = None

instance Foldable Option
    where
        foldMap :: Monoid m => (a -> m) -> Option a -> m
        foldMap _ None     = mempty
        foldMap f (Some a) = f a

instance Traversable Option
    where
        traverse :: (Applicative f  )=> (a -> f b) -> Option a -> f (Option b)
        traverse f (Some a) =  Some  <$> f a
        traverse _ None = pure None


-- SAMPLE REPL USAGES
-- ghci> import Data.Monoid
-- ghci> Some ( Sum  3) <> Some ( Sum 3)
-- ghci> Some (Sum {getSum = 6})
-- ghci> foldMap  id  $ Some "Tom"
-- ghci> foldMap  id  $ Some (Sum 12)
-- ghci> traverse id $ Some "Tom"
-- ghci> traverse id $ Some (Sum 12)

EOF

cat > src/Doodles.hs <<EOF
module 
	Doodles 
		where

main :: IO () 
main = do
    what <- life'sMeaning
    putStr "It is "
    print what
    return ()

life'sMeaning :: IO Integer
life'sMeaning = 
    humbleBeginnings >>= 
        stormAndStress >>= 
            enlightenment
    where
        humbleBeginnings :: IO Integer
        humbleBeginnings = readIO "0"

        stormAndStress :: Integer -> IO Integer
        stormAndStress i = let storm = readIO $ show i :: IO Integer
                               stress x = readIO ( "2" ++ show x ) 
                           in storm >>= stress

        enlightenment :: Integer -> IO Integer
        enlightenment i = readIO $ show (2 * i + 2)
   
EOF

cat > test/TestSuite.hs <<EOF
module 
	Main 
		where

import Lib
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Monoid

 -- ============= QUICKCHECK PROPERTIES 
prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool 
prop_monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)  

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a 

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a
 
main :: IO () 
main = do
  unitTests
  quickCheckTests

unitTests :: IO () 
unitTests = hspec $ do 
  
  describe "unit tests" $ do 

    it "Some (Sum 1) \`mappend\` Some (Sum 1)" $ do
      let n = Some (Sum 1) \`mappend\` Some (Sum 1)
      getSum <$> n \`shouldBe\` Some (2::Int)

    it "Some (Product 2) \`mappend\` Some (Product 3)" $ do
      let n = Some (Product 2) \`mappend\` Some (Product 3)
      getProduct <$> n \`shouldBe\` Some (6::Int)

    it "Some (Sum 1) \`mappend\` None" $ do
      let n = Some (Sum 1) \`mappend\` None
      getSum <$> n \`shouldBe\` Some (1::Int)

    it "None \`mappend\` Some (Sum 1)" $ do
      let n = None \`mappend\` Some (Sum 1)
      getSum <$> n \`shouldBe\` Some (1::Int)

    it "(Some J \`mappend\` ((Some a) \`mappend\` (Some n))" $ do
      let result = Some "J" \`mappend\` (Some "a" \`mappend\` Some "n")
      result \`shouldBe\` Some "Jan"

quickCheckTests :: IO () 
quickCheckTests = hspec $ do 
  
  describe "quick check tests" $ do

    it "associativity rule check of monoids with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidAssoc :: Sum Int -> Sum Int -> Sum Int -> Bool)
    it "associativity rule check of monoids with arbitrary String input" $ do
      quickCheck (prop_monoidAssoc :: String -> String -> String -> Bool)
    it "left identity rule check with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidLeftIdentity :: Sum Int -> Bool)
    it "right identity rule check with arbitrary Sum Int input" $ do
      quickCheck (prop_monoidRightIdentity :: Sum Int -> Bool)
      
EOF

cat > $project.cabal <<EOF
cabal-version:      3.0
name:               $project
version:            0.1.0.0
synopsis:           Synopsis of $project
license:            MIT
license-file:       LICENSE
author:             ja-cha
maintainer:         jan_abt@yahoo.com
build-type:         Simple
extra-doc-files:    CHANGELOG.md

common common-all
    ghc-options: -Wall -fwarn-tabs
    default-language: GHC2021
    build-depends:    base ^>=4.18.1.0 && < 5,
                      Cabal ^>=3.10.2.1
    
library
    import:           common-all
    exposed-modules:  Lib, Doodles
    hs-source-dirs:   src
    build-depends:    hspec ^>= 2.11.7,
                      QuickCheck ^>= 2.14.3,
                      checkers ^>=0.6.0 
                
executable monad-transformers
    import:           common-all
    main-is:          Main.hs
    other-modules:    Lib, Doodles
    hs-source-dirs:   app, src
    build-depends:    hspec ^>= 2.11.7,
                      QuickCheck ^>= 2.14.3

test-suite test-app
    import:           common-all
    other-modules:    Lib
    type:             exitcode-stdio-1.0
    main-is:          TestSuite.hs
    hs-source-dirs:   src, test
    build-depends:    hspec ^>= 2.11.7,
                      QuickCheck ^>= 2.14.3
EOF

cabal run

}

project="change-me" #$1
chapter="0"

# Check if two arguments were provided
if [ "$#" -ne 2 ]; then
    echo "project name and chapter number are required"
    echo "Usage: $0 my-project 12"
    exit 1  # Exit with an error code
fi

project=$1
chapter=$2


main

