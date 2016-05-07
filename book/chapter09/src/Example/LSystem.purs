module Example.LSystem where

import Prelude (class Monad, bind, return, flip, (-), (+), (*), (/), ($))
import Data.Array (concatMap, foldM)

import Data.Maybe (Maybe(Just))
import Control.Monad.Eff (Eff)
import Graphics.Canvas (Canvas, moveTo, lineTo
                       , getCanvasElementById, getContext2D
                       , setStrokeStyle, strokePath)

type Angle = Number
data Alphabet = L Angle | R Angle | F
type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

lsystem :: forall a m s. (Monad m) =>
                         Array a ->
                         (a -> Array a) ->
                         (a -> s -> m s) ->
                         Int ->
                         s -> m s
lsystem sentence prod step n state = go sentence n
  where
    go s 0 = foldM (flip step) state s
    go s n = go (concatMap prod s) (n - 1)

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    turn :: Angle
    turn = Math.pi / 3.0

    initial :: Sentence
    initial = [F, R turn, R turn, F, R turn, R turn, F, R turn, R turn]

    productions :: Alphabet -> Sentence
    productions (L x) = [L x]
    productions (R x) = [R x]
    productions F = [F, L turn, F, R (2.0 * turn), F, L turn, F]
    
    interpret :: Alphabet -> State -> Eff (canvas :: Canvas) State
    interpret (L angle) state = return $ state { theta = state.theta - angle }
    interpret (R angle) state = return $ state { theta = state.theta + angle }
    interpret F state = do
      let x' = state.x + Math.cos state.theta * 1.5
          y' = state.y + Math.sin state.theta * 1.5
      moveTo ctx state.x state.y
      lineTo ctx x' y'
      return { x: x', y: y', theta: state.theta }

    initialState :: State
    initialState = { x: 120.0, y: 200.0, theta: 0.0 }

  setStrokeStyle "#000000" ctx
  strokePath ctx $ lsystem initial productions interpret 5 initialState
