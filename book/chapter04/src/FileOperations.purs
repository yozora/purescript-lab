module FileOperations where

import Prelude

import Data.Array (concatMap, (:))
import Data.Path 

allFiles :: Path -> Array Path
allFiles path =
  path : concatMap allFiles (ls path)

allFiles' :: Path -> Array Path
allFiles' path =
  path : do
    file <- ls path
    allFiles' file

