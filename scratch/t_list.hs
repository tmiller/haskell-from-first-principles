module TList where

data TList a = End
             | Cons a (TList a)
  deriving (Show, Eq)

tHead :: TList a -> Maybe a
tHead (Cons x _) = Just x
tHead _          = Nothing

tTail :: TList a -> TList a
tTail (Cons _ xs) = xs
tTail _           = End

tmap :: (a -> b) -> TList a -> TList b
tmap _ End = End
tmap f (Cons x xs) = Cons (f x) (tmap f xs)
