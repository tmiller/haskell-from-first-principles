module Main where

import Test.QuickCheck
import Control.Monad (liftM)

data Optional a = Nada 
                | Only a
                deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where
  Only a <> Only a' = Only $ a <> a'
  Only a <> Nada    = Only a
  Nada   <> Only a  = Only a
  _      <> _       = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (3, liftM Only arbitrary)
                        ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' a@(Only _) <> First' (Only _)   = First' a
  First' a@(Only _) <> First' _          = First' a
  First' _          <> First' a@(Only _) = First' a
  _                 <> _                 = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary
    

type FirstMappend =
  First' String -> First' String -> First' String -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
