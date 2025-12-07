module Dungeon
  ( generateDungeon,
    findEmptySpace,
    getTile,
  )
where

import System.Random (StdGen, randomR)
import Types

-- A rectangular room
data Room = Room
  { roomX :: Int,
    roomY :: Int,
    roomWidth :: Int,
    roomHeight :: Int
  }
  deriving (Show)

-- Generate a dungeon with rooms and corridors
generateDungeon :: StdGen -> ([[Tile]], StdGen)
generateDungeon gen =
  let emptyDungeon = replicate dungeonHeight (replicate dungeonWidth Wall)
      (rooms, gen1) = generateRooms 8 gen []
      dungeonWithRooms = foldl carveRoom emptyDungeon rooms
      dungeonWithCorridors = carveCorridors dungeonWithRooms rooms
   in (dungeonWithCorridors, gen1)

-- Generate a list of non-overlapping rooms
generateRooms :: Int -> StdGen -> [Room] -> ([Room], StdGen)
generateRooms 0 gen rooms = (rooms, gen)
generateRooms n gen rooms =
  let (room, gen1) = generateRoom gen
      (room', gen2) = tryPlaceRoom room gen1 rooms 30 -- Try up to 30 times
   in case room' of
        Just r -> generateRooms (n - 1) gen2 (r : rooms)
        Nothing -> generateRooms (n - 1) gen2 rooms -- Give up on this room

-- Try to place a room, regenerating if it overlaps
tryPlaceRoom :: Room -> StdGen -> [Room] -> Int -> (Maybe Room, StdGen)
tryPlaceRoom _ gen _ 0 = (Nothing, gen) -- Give up after max attempts
tryPlaceRoom room gen rooms attempts =
  if any (overlaps room) rooms
    then
      let (newRoom, gen1) = generateRoom gen
       in tryPlaceRoom newRoom gen1 rooms (attempts - 1)
    else (Just room, gen)

-- Generate a single random room
generateRoom :: StdGen -> (Room, StdGen)
generateRoom gen =
  let (w, gen1) = randomR (6, 12) gen
      (h, gen2) = randomR (5, 8) gen1
      (x, gen3) = randomR (2, dungeonWidth - w - 2) gen2
      (y, gen4) = randomR (2, dungeonHeight - h - 2) gen3
   in (Room x y w h, gen4)

-- Check if two rooms overlap (with 1 tile buffer)
overlaps :: Room -> Room -> Bool
overlaps r1 r2 =
  not
    ( roomX r1 + roomWidth r1 + 1 < roomX r2
        || roomX r2 + roomWidth r2 + 1 < roomX r1
        || roomY r1 + roomHeight r1 + 1 < roomY r2
        || roomY r2 + roomHeight r2 + 1 < roomY r1
    )

-- Carve a room into the dungeon
carveRoom :: [[Tile]] -> Room -> [[Tile]]
carveRoom dungeon room =
  let coords =
        [ (x, y)
          | y <- [roomY room .. roomY room + roomHeight room - 1],
            x <- [roomX room .. roomX room + roomWidth room - 1]
        ]
   in foldl (\d (x, y) -> setTile d (Position x y) Floor) dungeon coords

-- Carve corridors between all adjacent rooms
carveCorridors :: [[Tile]] -> [Room] -> [[Tile]]
carveCorridors dungeon [] = dungeon
carveCorridors dungeon [_] = dungeon
carveCorridors dungeon (r1 : r2 : rest) =
  let dungeon' = carveCorridor dungeon (roomCenter r1) (roomCenter r2)
   in carveCorridors dungeon' (r2 : rest)

-- Get the center of a room
roomCenter :: Room -> Position
roomCenter room =
  Position
    (roomX room + roomWidth room `div` 2)
    (roomY room + roomHeight room `div` 2)

-- Carve an L-shaped corridor between two positions
carveCorridor :: [[Tile]] -> Position -> Position -> [[Tile]]
carveCorridor dungeon (Position x1 y1) (Position x2 y2) =
  let horizontalCoords = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
      verticalCoords = [(x2, y) | y <- [min y1 y2 .. max y1 y2]]
      allCoords = horizontalCoords ++ verticalCoords
   in foldl (\d (x, y) -> setTile d (Position x y) Floor) dungeon allCoords

-- Set a tile at a specific position
setTile :: [[Tile]] -> Position -> Tile -> [[Tile]]
setTile dungeon (Position px py) tile
  | py < 0 || py >= length dungeon = dungeon
  | px < 0 = dungeon
  | otherwise =
      let (before, rest) = splitAt py dungeon
       in case rest of
            [] -> dungeon
            (row : after) ->
              let (rowBefore, rowRest) = splitAt px row
               in case rowRest of
                    [] -> dungeon
                    (_ : rowAfter) ->
                      let newRow = rowBefore ++ [tile] ++ rowAfter
                       in before ++ [newRow] ++ after

-- Find a random empty floor space in the dungeon
findEmptySpace :: [[Tile]] -> StdGen -> (Position, StdGen)
findEmptySpace dungeon gen =
  let (px, gen1) = randomR (1, dungeonWidth - 2) gen
      (py, gen2) = randomR (1, dungeonHeight - 2) gen1
      pos = Position px py
   in if getTile dungeon pos == Floor
        then (pos, gen2)
        else findEmptySpace dungeon gen2

-- Get the tile at a specific position
getTile :: [[Tile]] -> Position -> Tile
getTile dungeon (Position px py)
  | py < 0 || py >= length dungeon = Wall
  | px < 0 || px >= length (head dungeon) = Wall
  | otherwise = dungeon !! py !! px
