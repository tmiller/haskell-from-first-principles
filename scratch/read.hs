module Read where

import Control.Applicative
import Control.Monad.Reader
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- Warming Up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  a <- cap
  b <- rev
  return (a, b)

-- Wrong
tupled'' :: [Char] -> ([Char], [Char])
tupled'' xs = rev <$> (cap >>= (,)) xs

tupled''' :: [Char] -> ([Char], [Char])
tupled''' =
  cap >>= (\a ->
    rev >>= (\b _ ->
        (a, b)))

-- Next Questions

-- asks :: Reader a a
-- asks = reader id

asks :: (r -> a) -> Reader r a
asks f = reader f

-- Example
newtype HumanName =
  HumanName String
  deriving(Eq, Show)

newtype DogName =
  DogName String
  deriving(Eq, Show)

newtype Address =
  Address String
  deriving(Eq, Show)

data Person =
  Person { humanName :: HumanName
         , dogName :: DogName
         , address :: Address
         } deriving (Eq, Show)

data Dog =
  Dog { dogsName :: DogName
      , dogsAddress :: Address
      } deriving (Eq, Show)

data Doggo =
  Doggo { doggoName :: DogName
        , doggoAddress :: Address
        , doggoOwner :: HumanName
        } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Seaseme Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)


getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDoggoR :: Person -> Doggo
getDoggoR =
  Doggo <$> dogName <*> address <*> humanName

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f g h = f <$> g <*> h
