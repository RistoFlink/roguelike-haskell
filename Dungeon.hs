module Dungeon
  ( generateDungeon,
    findEmptySpace,
    getTile,
  )
where

import System.Random (StdGen, randomR)
import Types

-- Generate random dungeon with walls and floors
generateDungeon :: StdGen -> ([[Tile]], StdGen)
generateDungeon gen =
  let (walls, gen') = makeRandomWalls gen
   in (makeBorder walls, gen')
  where
    -- Generate random interior tiles
    makeRandomWalls :: StdGen -> ([[Tile]], StdGen)
    makeRandomWalls g = makeRows dungeonHeight g

    -- Generate multiple rows
    makeRows :: Int -> StdGen -> ([[Tile]], StdGen)
    makeRows 0 g = ([], g)
    makeRows n g =
      let (row, g') = makeRow dungeonWidth g
          (rest, g'') = makeRows (n - 1) g'
       in (row : rest, g'')

    -- Generate a single row of tiles
    makeRow :: Int -> StdGen -> ([Tile], StdGen)
    makeRow 0 g = ([], g)
    makeRow n g =
      let (r, g') = randomR (0, 100) g :: (Int, StdGen)
          tile = if r < 20 then Wall else Floor
          (rest, g'') = makeRow (n - 1) g'
       in (tile : rest, g'')

    -- Add walls around the border
    makeBorder :: [[Tile]] -> [[Tile]]
    makeBorder rows =
      let topBottom = replicate dungeonWidth Wall
          middle = map addSideWalls rows
       in topBottom : take (dungeonHeight - 2) middle ++ [topBottom]

    -- Add walls to left and right of a row
    addSideWalls :: [Tile] -> [Tile]
    addSideWalls row = Wall : take (dungeonWidth - 2) row ++ [Wall]

-- Find a random empty floor space
findEmptySpace :: [[Tile]] -> StdGen -> (Position, StdGen)
findEmptySpace dungeon gen =
  let (px, gen1) = randomR (1, dungeonWidth - 2) gen
      (py, gen2) = randomR (1, dungeonHeight - 2) gen1
      pos = Position px py
   in if getTile dungeon pos == Floor
        then (pos, gen2)
        else findEmptySpace dungeon gen2

-- Get the tile at specfic position
getTile :: [[Tile]] -> Position -> Tile
getTile dungeon (Position px py)
  | py < 0 || py >= length dungeon = Wall
  | px < 0 || px >= length (head dungeon) = Wall
  | otherwise = dungeon !! py !! px
