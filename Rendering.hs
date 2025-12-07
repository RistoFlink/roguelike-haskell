module Rendering
  ( renderGame,
    clearScreen,
  )
where

import Control.Monad (forM_)
import Data.List (find)
import Data.Maybe (isJust)
import Types

-- Render the entire game state to the terminal
renderGame :: GameState -> IO ()
renderGame state = do
  clearScreen
  -- Display stats
  putStrLn $
    "HP: "
      ++ show (playerHealth state)
      ++ "/"
      ++ show (playerMaxHealth state)
      ++ " | ATK: "
      ++ show (playerAttack state)
      ++ " | Monsters: "
      ++ show (length $ monsters state)
  putStrLn (replicate dungeonWidth '-')

  -- Display the dungeon map
  forM_ [0 .. dungeonHeight - 1] $ \y -> do
    forM_ [0 .. dungeonWidth - 1] $ \x -> do
      let pos = Position x y
      putChar $ getCharAtPosition pos state
    putStrLn ""

  putStrLn (replicate dungeonWidth '-')
  putStrLn $ message state
  if gameOver state
    then putStrLn "\nGAME OVER"
    else putStrLn "\nControls: WASD to move, Q to quit"

-- Determine what character to display at a position
getCharAtPosition :: Position -> GameState -> Char
getCharAtPosition pos state
  | pos == playerPos state = '@'
  | isJust monsterHere = getMonsterChar (fromJust monsterHere)
  | isJust itemHere = getItemChar (fromJust itemHere)
  | otherwise = getTileChar (getTile (dungeon state) pos)
  where
    monsterHere = find (\m -> mPos m == pos) (monsters state)
    itemHere = find (\i -> iPos i == pos) (items state)
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"

-- Get character representation for a monster
getMonsterChar :: Monster -> Char
getMonsterChar monster = case mType monster of
  Goblin -> 'g'
  Orc -> 'O'
  _ -> '?'

-- Get character representation for an item
getItemChar :: Item -> Char
getItemChar item = case iType item of
  Potion -> '!'
  Sword -> '/'
  _ -> '?'

-- Get character representation for a tile
getTileChar :: Tile -> Char
getTileChar Floor = '.'
getTileChar Wall = '#'
getTileChar Door = '+'

-- Helper function to get tile (imported from Dungeon via Types)
getTile :: [[Tile]] -> Position -> Tile
getTile dungeon (Position px py)
  | py < 0 || py >= length dungeon = Wall
  | px < 0 || px >= length (head dungeon) = Wall
  | otherwise = dungeon !! py !! px

-- Clear the terminal screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"
