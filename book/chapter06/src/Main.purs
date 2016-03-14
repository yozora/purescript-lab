module Main where

import Prelude (class Eq, class Functor, class Semigroup, class Show, class Ord,
                Unit, Ordering(GT, LT), map, (++), (==), (&&), (<$>),
                show, compare, eq) 
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Monoid (class Monoid)

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq (Finite a) (Finite b) = eq a b
  eq _ _ = false
  
instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite a) (Finite b) = compare a b

instance showExtended :: (Show a) => Show (Extended a) where
  show (Finite a) = "(Finite " ++ (show a) ++ ")"
  show Infinite = "Infinite"


newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance eqComplex :: Eq Complex where
  eq (Complex a) (Complex b) = (a.real == b.real) && (a.imaginary == b.imaginary)

instance showComplex :: Show Complex where
  show (Complex a) = "(Complex { real: " ++ (show a.real)
    ++ ", imaginary: " ++ (show a.imaginary) ++ "}"
  

data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty head tail) =
    "(NonEmpty " ++ (show head) ++ " " ++ (show tail) ++ ")"
    
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty lf la) (NonEmpty rf ra) =
    NonEmpty lf (la ++ [rf] ++ ra)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty head tail) =
    NonEmpty (f head) (f <$> tail)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f s (NonEmpty x xs) = f x (foldr f s xs)
  foldl f s (NonEmpty x xs) = foldl f (f s x) xs
  foldMap f (NonEmpty x xs) = (f x) ++ (foldMap f xs)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x == y) && (xs == ys)


data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr f s (OneMore x xs) = f x (foldr f s xs)
  foldl f s (OneMore x xs) = foldl f (f s x) xs
  foldMap f (OneMore x xs) = (f x) ++ (foldMap f xs)

class (Monoid m) <= Action m a where
  act :: m -> a -> a

instance actionArray :: (Action m a) => Action m (Array a) where
  act m = map (act m)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
 log "Hello sailor!"
