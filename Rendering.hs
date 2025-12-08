module Rendering
  ( renderGame,
    clearScreen,
    hideCursor,
    showCursor,
  )
where

import Data.List (find)
import Data.Maybe (isJust)
import Types

-- ANSI color codes
resetColor :: String
resetColor = "\ESC[0m"

red, green, yellow, blue, magenta, cyan, white, brightRed :: String -> String
red s = "\ESC[31m" ++ s ++ resetColor
green s = "\ESC[32m" ++ s ++ resetColor
yellow s = "\ESC[33m" ++ s ++ resetColor
blue s = "\ESC[34m" ++ s ++ resetColor
magenta s = "\ESC[35m" ++ s ++ resetColor
cyan s = "\ESC[36m" ++ s ++ resetColor
white s = "\ESC[37m" ++ s ++ resetColor
brightRed s = "\ESC[91m" ++ s ++ resetColor

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ resetColor

-- Render the entire game state to the terminal
renderGame :: GameState -> IO ()
renderGame state = do
  let header =
        bold "HP: "
          ++ (if playerHealth state < 5 then brightRed else green)
            (show (playerHealth state) ++ "/" ++ show (playerMaxHealth state))
          ++ bold " | ATK: "
          ++ cyan (show (playerAttack state))
          ++ bold " | Monsters: "
          ++ red (show (length $ monsters state))
          ++ clearRestOfLine

      separator = replicate dungeonWidth '-' ++ clearRestOfLine

      mapLines =
        [ concat [getCharAtPosition (Position x y) state | x <- [0 .. dungeonWidth - 1]] ++ clearRestOfLine
          | y <- [0 .. dungeonHeight - 1]
        ]

      msg = message state ++ clearRestOfLine
      
      footer = 
        if gameOver state
          then red "GAME OVER" ++ clearRestOfLine
          else bold "Controls: WASD to move, Q to quit" ++ clearRestOfLine

      frame =
        hideCursor
          ++ "\ESC[H" -- Move cursor to home
          ++ unlines ([header, separator] ++ mapLines ++ [separator, msg, clearRestOfLine, footer])

  putStr frame

-- Determine what character to display at a position
getCharAtPosition :: Position -> GameState -> String
getCharAtPosition pos state
  | pos == playerPos state = bold $ yellow "@"
  | isJust monsterHere = getMonsterChar (fromJust monsterHere)
  | isJust itemHere = getItemChar (fromJust itemHere)
  | otherwise = getTileChar (getTile (dungeon state) pos)
  where
    monsterHere = find (\m -> mPos m == pos) (monsters state)
    itemHere = find (\i -> iPos i == pos) (items state)
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"

-- Get character representation for a monster (with color)
getMonsterChar :: Monster -> String
getMonsterChar monster = case mType monster of
  Goblin -> green "g"
  Orc -> red "O"
  _ -> "?"

-- Get character representation for an item (with color)
getItemChar :: Item -> String
getItemChar item = case iType item of
  Potion -> green "!"
  Sword -> cyan "/"
  _ -> "?"

-- Get character representation for a tile (with color)
getTileChar :: Tile -> String
getTileChar Floor = blue "."
getTileChar Wall = white "#"
getTileChar Door = yellow "+"

-- Helper function to get tile (imported from Dungeon via Types)
getTile :: [[Tile]] -> Position -> Tile
getTile dungeon (Position px py)
  | py < 0 || py >= length dungeon = Wall
  | px < 0 || px >= length (head dungeon) = Wall
  | otherwise = dungeon !! py !! px

-- Clear the terminal screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[H"

-- Clear the rest of the current line
clearRestOfLine :: String
clearRestOfLine = "\ESC[K"

-- Hide the cursor
hideCursor :: String
hideCursor = "\ESC[?25l"

-- Show the cursor
showCursor :: String
showCursor = "\ESC[?25h"
