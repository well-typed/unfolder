{-# LANGUAGE UndecidableInstances #-}

module Show where

import Data.List
import Data.Kind
import Data.Proxy

class MyShow a where
  myshow :: a -> String

instance MyShow Char where myshow = show
instance MyShow Bool where myshow = show

instance MyShow a => MyShow [a] where
  myshow xs = "[" ++ intercalate "," (map myshow xs) ++ "]"

instance {-# OVERLAPPING #-} MyShow String where
  myshow xs = "\"" ++ xs ++ "\""

example xs = myshow (xs ++ xs)

-- >>> myshow [False, True, False]
-- "[False,True,False]"
-- >>> example "unfolder"
-- "\"unfolderunfolder\""
