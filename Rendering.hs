module Rendering
  ( renderApp, -- Renamed from renderGame
    clearScreen,
    hideCursor,
    showCursor,
  )
where

import Ancestry
import Data.List (find)
import Data.Set qualified as Set
import Stats
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

dimmed :: String -> String
dimmed s = "\ESC[90m" ++ s ++ resetColor

-- Top-level render function for the App
renderApp :: App -> IO ()
renderApp app = case currentScreen app of
  MainMenu -> renderMainMenu
  Playing -> case gameState app of
    Just gs -> renderGame gs
    Nothing -> putStrLn "Error: Playing state but no GameState!"
  GameOverScreen -> renderGameOverScreen (gameState app)
  CharacterCreation -> case creation app of
    Just cs -> renderCharacterCreation cs
    Nothing -> putStrLn "Error: creation state but no CreationState"
  Exit -> return ()

-- Render the Main Menu
renderMainMenu :: IO ()
renderMainMenu = do
  clearScreen
  putStrLn $ hideCursor ++ "\ESC[H"
  putStrLn $ bold $ yellow "  ROGUELIKE HASKELL  "
  putStrLn ""
  putStrLn "  [n] New Game"
  putStrLn "  [q] Quit"
  putStrLn ""
  putStrLn "  Select an option..."

-- Render the Game Over Screen
renderGameOverScreen :: Maybe GameState -> IO ()
renderGameOverScreen maybeGs = do
  clearScreen
  putStrLn $ hideCursor ++ "\ESC[H"
  putStrLn $ bold $ red "      GAME OVER      "
  putStrLn ""
  case maybeGs of
    Just gs -> putStrLn $ "  You died." -- Depth stats can go here later
    Nothing -> return ()
  putStrLn ""
  putStrLn "  [r] Restart"
  putStrLn "  [q] Return to Main Menu"

-- Render the character creation menu
renderCharacterCreation :: CreationState -> IO ()
renderCharacterCreation cs = do
  clearScreen
  putStrLn $ hideCursor ++ "\ESC[H"
  putStrLn $ bold $ yellow " CHARACTER CREATION "
  putStrLn ""

  -- Sidebar to display stats
  let renderSidebar stats = do
        putStr "\ESC[2;30H" -- Move cursor to row 2, col 30
        putStrLn $ bold $ cyan "  CURRENT STATS  "
        putStr "\ESC[3;30H"
        putStrLn $ "  Str: " ++ show (str stats)
        putStr "\ESC[4;30H"
        putStrLn $ "  Dex: " ++ show (dex stats)
        putStr "\ESC[5;30H"
        putStrLn $ "  Con: " ++ show (con stats)
        putStr "\ESC[6;30H"
        putStrLn $ "  Int: " ++ show (int stats)
        putStr "\ESC[7;30H"
        putStrLn $ "  Wis: " ++ show (wis stats)
        putStr "\ESC[8;30H"
        putStrLn $ "  Cha: " ++ show (cha stats)
        putStr "\ESC[H" -- Reset cursor to top for the main menu
  renderSidebar (currentStats cs)

  case currentStep cs of
    PickAncestry -> do
      putStrLn " Pick your Ancestry:"
      mapM_ (renderAncestryChoice (selectedIndex cs)) (zip [0 ..] playableAncestries)
    _ -> putStrLn " Next step (TODO)"

renderAncestryChoice :: Int -> (Int, Ancestry) -> IO ()
renderAncestryChoice currentIdx (idx, anc) =
  let highlight = if idx == currentIdx then bold (yellow "> ") else " "
   in putStrLn $ highlight ++ show anc

-- Render the dungeon simulation (Existing logic)
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

      currentVisible = getVisibleTiles state
      allExplored = exploredTiles state

      mapLines =
        [ concat
            [ getCharForFogOfWar (Position x y) state currentVisible allExplored
              | x <- [0 .. dungeonWidth - 1]
            ]
            ++ clearRestOfLine
          | y <- [0 .. dungeonHeight - 1]
        ]

      msg = message state ++ clearRestOfLine

      footer = bold "Controls: WASD to move, Q to quit" ++ clearRestOfLine

      frame =
        hideCursor
          ++ "\ESC[H" -- Move cursor to home
          ++ unlines ([header, separator] ++ mapLines ++ [separator, msg, clearRestOfLine, footer])

  putStr frame

-- Determine what character to display at a position (Fog of War)
getCharForFogOfWar :: Position -> GameState -> Set.Set Position -> Set.Set Position -> String
getCharForFogOfWar pos state currentVisible allExplored
  | Set.member pos currentVisible =
      if pos == playerPos state
        then bold (yellow "@")
        else case find (\m -> mPos m == pos) (monsters state) of
          Just monster -> getMonsterChar monster
          Nothing -> case find (\i -> iPos i == pos) (items state) of
            Just item -> getItemChar item
            Nothing -> getTileChar (getTile (dungeon state) pos)
  | Set.member pos allExplored =
      case getTile (dungeon state) pos of
        Wall -> "\ESC[38;5;240m#" ++ resetColor
        Floor -> "\ESC[38;5;240m." ++ resetColor
        Door -> "\ESC[38;5;240m+" ++ resetColor
  | otherwise =
      " "

-- Get character representation for a monster (with color)
getMonsterChar :: Monster -> String
getMonsterChar monster = case mType monster of
  Enemy Goblin -> green "g"
  Enemy Orc -> red "O"
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

-- Calculate currently visible tiles based on player position and radius
getVisibleTiles :: GameState -> Set.Set Position
getVisibleTiles state =
  let px = x (playerPos state)
      py = y (playerPos state)
      radius = 5

      visible =
        [ Position (px + dx) (py + dy)
          | dx <- [-radius .. radius],
            dy <- [-radius .. radius],
            (dx * dx + dy * dy) <= (radius * radius)
        ]
   in Set.fromList visible

-- Clear the terminal screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Clear the rest of the current line
clearRestOfLine :: String
clearRestOfLine = "\ESC[K"

-- Hide the cursor
hideCursor :: String
hideCursor = "\ESC[?25l"

-- Show the cursor
showCursor :: String
showCursor = "\ESC[?25h"
