module Main where

import Prelude
import Math (sqrt)
import Control.Monad.Eff.Console (log)

diagonal w h =
  sqrt (w * w + h * h)

main =
  log "Hello, World!!"
