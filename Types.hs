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

import System.Random (StdGen)

-- Position in the dungeon
data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

-- Tile types in the dungeon
data Tile
  = Floor
  | Wall
  | Door
  deriving (Eq, Show)
