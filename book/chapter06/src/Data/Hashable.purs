module Data.Hashable where

import Prelude
import Data.Monoid (class Monoid)
import Data.Function (on)
import Data.Foldable (foldMap)

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode i = HashCode ( i `mod` 65536)

instance eqHashCode :: Eq HashCode where
  eq (HashCode h1) (HashCode h2) = h1 `eq` h2
  
instance semigroupHashCode :: Semigroup HashCode where
  append (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

instance monoidHashCode :: Monoid HashCode where
  mempty = hashCode 0

class (Eq a) <= Hashable a where
  hash :: a -> HashCode

instance hashableInt :: Hashable Int where
  hash = hashCode

instance hashableBoolean :: Hashable Boolean where
  hash false = HashCode 0
  hash _ = HashCode 1

instance hashableArray :: (Hashable a) => Hashable (Array a) where
  hash = foldMap hash
  
hashEqual :: forall a. (Hashable a) => a -> a -> Boolean
hashEqual = eq `on` hash
