module Data.Game
       ( PlayerName
       , Coords(..)
       , GameDirection(..)
       , GameEnvironment(..)
       , GameItem(..)
       , GameState(..)
       , coords
       , gameEnvironment
       , readItem
       , initialGameState
       ) where

import Prelude ( (++), ($), (<>), (&&)
               , Ordering(..)
               , class Show, show, class Eq, eq
               , class Ord, compare )
import Data.Maybe (Maybe(Just, Nothing))

import Data.Tuple (Tuple(..))
import Data.List as L
import Data.Map as M
import Data.Set as S

type PlayerName = String

data GameDirection = North | South | East | West

newtype Coords = Coords
  { x :: Int
  , y :: Int
  }

coords :: Int -> Int -> Coords
coords x y = Coords { x: x, y: y }

instance eqCoords :: Eq Coords where
  eq (Coords c1) (Coords c2) =
    (eq c1.x c2.x) && (eq c1.y c2.y)
    
instance ordCoords :: Ord Coords where
  compare (Coords c1) (Coords c2) =
    (compare c1.x c2.x) <> (compare c1.y c2.y)

instance showCoords :: Show Coords where
  show (Coords p) = "Coords " ++
                    "{ x: " ++ show p.x ++
                    ", y: " ++ show p.y ++
                    " }"

newtype GameEnvironment = GameEnvironment
  { playerName :: PlayerName
  , debugMode :: Boolean
  }

gameEnvironment :: PlayerName -> Boolean -> GameEnvironment
gameEnvironment playerName debug = GameEnvironment
  { playerName : playerName
  , debugMode : debug
  }

data GameItem = Candle | Matches

instance eqGameItem :: Eq GameItem where
  eq Candle Candle = true
  eq Matches Matches = true
  eq _ _ = false

instance ordGameItem :: Ord GameItem where
  compare Candle Candle = EQ
  compare Candle Matches = LT
  compare Matches Candle = GT
  compare Matches Matches = EQ

instance showGameItem :: Show GameItem where
  show Candle = "(GameItem Candle)"
  show Matches = "(GameItem Matches)"

readItem :: String -> Maybe GameItem
readItem "candle" = Just Candle
readItem "matches" = Just Matches
readItem _ = Nothing

newtype GameState = GameState
  { items :: M.Map Coords (S.Set GameItem)
  , player :: Coords
  , inventory :: S.Set GameItem
  , gameRunning :: Boolean
  }

initialGameState :: GameState
initialGameState = GameState
  { items : M.fromList $ L.toList [ Tuple (coords 0 1) (S.singleton Candle)
                                  , Tuple (coords 0 0) (S.singleton Matches)
                                  ]
  , player : Coords { x: 0, y: 0 }
  , inventory : S.empty
  , gameRunning : true
  }
