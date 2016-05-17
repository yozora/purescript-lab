module Data.HRec ( HRec
                 , empty, insert, foldHRec, union, lookup
                 ) where

import Prelude (class Functor, class Show, show, (++))
import Data.Maybe (Maybe(Just,Nothing))
import Data.Function ( Fn3, runFn3, mkFn3, Fn4, runFn4
                     , Fn2, runFn2)

foreign import data HRec :: * -> *

foreign import empty :: forall a. HRec a

foreign import insert :: forall a. Fn3 String a (HRec a) (HRec a)

foreign import mapHRec :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)

foreign import foldHRecImpl :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r

foldHRec :: forall a r. (r -> String -> a -> r) -> r -> HRec a -> r
foldHRec fn seed r = runFn3 foldHRecImpl (mkFn3 fn) seed r

foreign import lookupImpl :: forall y v. Fn4 y (v -> y) String (HRec v) y 

lookup :: forall v. String -> (HRec v) -> (Maybe v)
lookup key record =
  runFn4 lookupImpl Nothing Just key record

union :: forall a. (HRec a) -> (HRec a) -> (HRec a)
union r1 r2 =
  runFn3 foldHRecImpl combine r1 r2
    where
      combine = mkFn3 \r k v -> runFn3 insert k v r

instance showHRec :: (Show a) => Show (HRec a) where
  show hrec = runFn3 foldHRecImpl f "empty" hrec
    where
      f = mkFn3 \s k a -> "insert " ++ (show k) ++ " " ++ (show a) ++ " $ " ++ s
      
instance functorHRec :: Functor HRec where
  map f record = runFn2 mapHRec f record
