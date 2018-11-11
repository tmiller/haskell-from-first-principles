{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Goats where

newtype Goats = Goats Int deriving (Eq, Show)
-- newtype Cows = Cows (Int, String) deriving (Eq, Show)

class TooMany a where
  toMany :: a -> Bool

-- instance TooMany Int where
--   toMany x = x > 42
-- 
-- instance TooMany Cows where
--   toMany (Cows (n, _)) = n > 42
-- 
-- instance TooMany (Int, Int) where
--   toMany (x, y) = (x + y) > 42

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
  toMany (x, y) = (x + y) > 10

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)
