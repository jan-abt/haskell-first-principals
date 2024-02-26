

module CaseExpressions where


 myMax x y = if (x > y) then x else y   
 myMax' x y = 
   case x > y of  
     True -> x
     False -> y

 add10IfEven n = if even n then (n+10) else n   
 add10IfEven' n = 
   case even n of
     True -> (n + 10)
     False -> (n)

 nums x =
   case compare x 0 of
     LT -> -1 
     GT -> 1
     EQ -> 0
