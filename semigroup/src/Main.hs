module Main where

import Test.QuickCheck
import Data.Monoid
-- import GHC.Generics

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance Show (Combine a b) where
  show _  = "Combine a b"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    a <- arbitrary
    return (Combine a)


semiGroupoAssoc :: (Eq b, Show b, Semigroup b)
                => a
                -> Combine a b
                -> Combine a b
                -> Combine a b
                -> Bool
semiGroupoAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)
 
type CombineAssoc =
     Integer
  -> Combine Integer (Sum Integer)
  -> Combine Integer (Sum Integer)
  -> Combine Integer (Sum Integer)
  -> Bool

main :: IO ()
main = do
   return ()
   quickCheck (semiGroupoAssoc :: CombineAssoc)
