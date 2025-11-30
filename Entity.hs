module Entity
  ( spawnMonsters,
    spawnItems,
    moveMonsters,
    pickupItems,
  )
where

import Data.List (delete)
import Dungeon (findEmptySpace, getTile)
import System.Random (StdGen, randomR)
import Types

spawnMonsters :: [[Tile]] -> Int -> StdGen -> ([Monster], StdGen)
