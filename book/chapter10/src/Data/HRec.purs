module Data.HRec ( HRec
                 , empty, insert, union
                 ) where

import Prelude (class Functor, class Show, show
               , (++), ($))
import Data.Function ( Fn3, runFn3, mkFn3
                     , Fn2, runFn2)

foreign import data HRec :: * -> *

foreign import empty :: forall a. HRec a

foreign import insert :: forall a. Fn3 String a (HRec a) (HRec a)

foreign import mapHRec :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)

foreign import foldHRec :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r

union :: forall a. (HRec a) -> (HRec a) -> (HRec a)
union r1 r2 =
  runFn3 foldHRec combine r1 r2
    where
      combine = mkFn3 \r k v -> runFn3 insert k v r

instance showHRec :: (Show a) => Show (HRec a) where
  show hrec = runFn3 foldHRec f "empty" hrec
    where
      f = mkFn3 \s k a -> "insert " ++ (show k) ++ " " ++ (show a) ++ " $ " ++ s
      
instance functorHRec :: Functor HRec where
  map f record = runFn2 mapHRec f record
