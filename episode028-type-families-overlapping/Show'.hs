{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Show' where

import Data.List
import Data.Kind
import Data.Proxy

class MyShow a where
  myshow :: a -> String

instance MyShow Char where myshow = show
instance MyShow Bool where myshow = show

instance MyShow' (IsString [a]) [a] => MyShow [a] where
  myshow = myshow' (Proxy @(IsString [a]))

type family IsString (a :: Type) :: Bool where
  IsString String = True
  IsString _ = False

class MyShow' (b :: Bool) a where
  myshow' :: Proxy b -> a -> String

instance MyShow a => MyShow' False [a] where
  myshow' _ xs = "[" ++ intercalate "," (map myshow xs) ++ "]"

-- instance MyShow' True String where
instance a ~ Char => MyShow' True [a] where
  myshow' _ xs = "\"" ++ xs ++ "\""


example' :: MyShow [a] => [a] -> String
example' xs = myshow (xs ++ xs)

-- >>> myshow [False, True, False]
-- "[False,True,False]"
-- >>> example' "unfolder"
-- "\"unfolderunfolder\""
