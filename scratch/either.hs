data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where

  fmap f (Second x) = Second (f x)
  fmap _ (First a)  = First a


instance Applicative (Sum a) where

  pure = Second

  (<*>) (First  a) = const (First a)
  (<*>) (Second f) = fmap f

instance Monad (Sum a) where

  return = pure

  (>>=) (First  a) _ = First a
  (>>=) (Second x) f = f x
