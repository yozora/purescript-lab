module Exercises (combineMaybe)
       where

import Prelude (class Applicative, pure, (<$>))
import Data.Maybe (Maybe(..))

combineMaybe :: forall f a. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just x) = Just <$> x
