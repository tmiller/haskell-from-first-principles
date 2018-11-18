module Optional where

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
