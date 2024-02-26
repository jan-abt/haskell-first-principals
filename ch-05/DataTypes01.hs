module DataTypes01 where

data Mood = Blah | Woot deriving Show
  
main :: IO () 
main = do
  return ()   

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah
