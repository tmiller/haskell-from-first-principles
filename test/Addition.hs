module Addition where

import Test.Hspec
import Test.QuickCheck

data Foo = One | Two | Three deriving (Show, Eq, Ord)

trivialFoo :: Gen Foo
trivialFoo = elements [One, Two, Three]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

test :: IO ()
test = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      (2 + 2) `shouldBe` 4
    it "x + 1 is always greater\
       \ then x" $ do
         property $ \x -> x + 1 > (x ::Int)

