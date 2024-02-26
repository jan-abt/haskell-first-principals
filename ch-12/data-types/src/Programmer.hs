module Programmer where

data OperatingSystem =
  GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
 Haskell
 | Agda
 | Idris
 | PureScript
 deriving (Eq, Show)

data Programmer =
 Programmer {
  os :: OperatingSystem,
  lang :: ProgrammingLanguage
 }
 deriving (Eq, Show)

nineToFive :: Programmer
nineToFive =
 Programmer {
  os = Mac,
  lang = Haskell
 }

-- We can reorder stuff when we use record syntax
feelingWizardly :: Programmer
feelingWizardly =
 Programmer {
  lang = Agda,
  os = GnuPlusLinux
 }

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
 [ 
   GnuPlusLinux, 
   OpenBSD, 
   Mac, 
   Windows
 ]

allLanguages :: [ProgrammingLanguage] 
allLanguages = 
 [
  Haskell, 
  Agda,
  Idris,
  PureScript
 ]
-- Programmer is a "product" of OperatingSystem and ProgrammingLanguage! 
-- Therefore, you can determine how many inhabitants of Programmer you have
programmerArity = length allOperatingSystems * length allLanguages

allProgrammers :: [Programmer] 
allProgrammers = [Programmer o l| o<-allOperatingSystems, l<-allLanguages  ]



