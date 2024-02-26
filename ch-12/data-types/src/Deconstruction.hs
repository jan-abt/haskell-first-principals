module Deconstruction  where


newtype Name    = Name String deriving Show
newtype Acres   = Acres Int deriving Show

-- FarmerType is a Sum type, a sum of disjuncted values
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

-- Farmer is a Product type, a product of -- Name, Acres, and FarmerType
data Farmer =
 Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True 
isDairyFarmer _ = False

-- alternate formulation of a product type, that uses record syntax
data FarmerRec =
 FarmerRec { 
     name :: Name, 
     acres :: Acres, 
     farmerType :: FarmerType 
 } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool 
isDairyFarmerRec farmerRec = 
 case (farmerType farmerRec) of
  DairyFarmer -> True
  _           -> False

