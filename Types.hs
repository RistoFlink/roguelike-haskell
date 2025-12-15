module Types
  ( Position (..),
    Tile (..),
    Entity (..),
    Monster (..),
    Item (..),
    GameState (..),
    dungeonWidth,
    dungeonHeight,
  )
where

import Data.Set qualified as Set
import System.Random (StdGen)

-- Position in the dungeon
data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

-- Tile types in the dungeon
data Tile
  = Floor
  | Wall
  | Door
  deriving (Eq, Show)

-- Type of entities (monsters, items, etc.)
data Entity
  = Player
  | Goblin
  | Orc
  | Potion
  | Sword
  deriving (Eq, Show)

-- Monster with position, type, and health
data Monster = Monster
  { mPos :: Position,
    mType :: Entity,
    mHealth :: Int
  }
  deriving (Eq, Show)

-- Item with position and type
data Item = Item
  { iPos :: Position,
    iType :: Entity
  }
  deriving (Eq, Show)

-- Complete game state
data GameState = GameState
  { playerPos :: Position,
    playerHealth :: Int,
    playerMaxHealth :: Int,
    playerAttack :: Int,
    dungeon :: [[Tile]],
    monsters :: [Monster],
    items :: [Item],
    message :: String,
    gameOver :: Bool,
    rng :: StdGen,
    exploredTiles :: Set.Set Position
  }
  deriving (Show)

-- Constants
dungeonWidth :: Int
dungeonWidth = 50

dungeonHeight :: Int
dungeonHeight = 20
