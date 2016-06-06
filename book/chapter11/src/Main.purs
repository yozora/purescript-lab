module Main where

import Prelude ( Unit, unit, bind, return, show, map
               , (+), (-)
               , ($), (#), (<<<), (++), (<$>), (<*>))

import Data.Game ( GameEnvironment, GameState(..), GameItem
                 , GameDirection(North,South)
                 , Coords(..), coords, gameEnvironment
                 , initialGameState )

import Data.Either (Either(Right))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.List as L
import Data.Set as S
import Data.Map as M

import Control.Monad.RWS (RWS, get, put, modify, tell, runRWS)
import Control.Monad.RWS.Trans (RWSResult(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

type Log = L.List String

import Node.ReadLine as RL
import Node.Yargs.Applicative as Y
import Node.Yargs.Setup as YS

type Game = RWS GameEnvironment Log GameState

-- Game Data

newtype MapCell = MapCell { description :: String }

type GameMap = M.Map Coords MapCell 

gameMap :: GameMap
gameMap =
  M.empty
  # M.insert
    (coords 0 0)
    (MapCell
       { description: "You are in a forest. You see a path to the north."
       })
  # M.insert (coords 0 1) (MapCell { description: "You are in a clearing."})
            
-- Game Logic

move :: GameDirection -> Game Unit
move dir = do
  GameState state <- get
  modify (\(GameState state) -> GameState state { player = updatePlayer state.player })
  where
    updatePlayer (Coords p) =
      case dir of
        North -> (coords p.x (p.y + 1))
        South -> (coords p.x (p.y - 1))
        _ -> (coords p.x p.y)

describeRoom :: Game Unit
describeRoom = do
  GameState state <- get
  case M.lookup (state.player) gameMap of
    Just (MapCell c) -> tell $ L.singleton c.description
    Nothing -> tell $ L.singleton "You are lost in the forest."

has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  return $ item `S.member` state.inventory

pickup :: GameItem -> Game Unit
pickup item = do
  GameState state <- get
  case state.player `M.lookup` state.items of
    Just items | item `S.member` items -> do
      let newItems = M.update (Just <<< S.delete item) state.player state.items
          newInventory = S.insert item state.inventory
      put $ GameState state { items = newItems
                            , inventory = newInventory
                            }
      tell $ L.singleton $ "You now have the " ++ (show item)
    _ -> tell $ L.singleton $ "I don't see that item here"

type See s a w = { log :: w, result :: a, state :: s }

convertResult :: forall s a w. RWSResult s a w -> See s a w
convertResult (RWSResult s a w) =
  { log : w
  , result : a
  , state : s }

game :: Array String -> Game Unit
game ["look"] = describeRoom
game ["move", "north"] = move North
game ["move", "south"] = move South
game ["quit"] = do
  GameState state <- get
  put $ GameState state { gameRunning = false }
game _ = tell (L.singleton "I don't understand")

runGame :: forall eff. GameEnvironment -> Eff ( console :: CONSOLE
                                              , readline :: RL.READLINE
                                              , err :: EXCEPTION | eff) Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " 2 interface
  
  let
    lineHandler :: forall eff1. GameState ->
                                String ->
                                Eff ( readline :: RL.READLINE
                                    , console :: CONSOLE | eff1) Unit
    lineHandler currentState input = do
      let result = convertResult $ runRWS (game (split " " input)) env currentState
      for_ result.log log
      case result.state of
        (GameState s) | s.gameRunning ->
          do
            RL.setLineHandler interface $ lineHandler result.state
            RL.prompt interface
        _ ->
          do
            RL.close interface 
      return unit
          

  RL.setLineHandler interface $ lineHandler initialGameState
  RL.prompt interface

  return unit
  
main :: forall e. Eff ( console :: CONSOLE
                      , readline :: RL.READLINE
                      , err :: EXCEPTION | e) Unit
main = Y.runY (YS.usage "$0 -p <player name>")
              (map runGame env)
  where
    env :: Y.Y GameEnvironment
    env = gameEnvironment
          <$> Y.yarg "p" ["player"] (Just "Player name") (Right "Player name required") false
          <*> Y.flag "d" ["debug"] (Just "Use debug mode")
          
-- main = do
--   runGame $ GameEnvironment
--     { playerName : "Yozora"
--     , debugMode : true
--     }

