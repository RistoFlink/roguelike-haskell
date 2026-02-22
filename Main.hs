module Main where

import Combat (movePlayer)
import Data.Set qualified as Set
import Dungeon (findEmptySpace, generateDungeon)
import Entity (spawnItems, spawnMonsters)
import Rendering (renderApp, showCursor)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, stdout)
import System.Random (getStdGen)
import Types

-- Initialize the top-level application
initApp :: IO App
initApp = return $ App {currentScreen = MainMenu, gameState = Nothing}

-- Initialize a new game state (simulation)
initGame :: IO GameState
initGame = do
  gen <- getStdGen
  let (dungeon', gen1) = generateDungeon gen
      (playerPos', gen2) = findEmptySpace dungeon' gen1
      (monsters', gen3) = spawnMonsters dungeon' 8 gen2
      (items', gen4) = spawnItems dungeon' 5 gen3
  return
    GameState
      { playerPos = playerPos',
        playerHealth = 20,
        playerMaxHealth = 20,
        playerAttack = 3,
        dungeon = dungeon',
        monsters = monsters',
        items = items',
        message = "Welcome! Use WASD to move.",
        gameOver = False,
        rng = gen4,
        exploredTiles = Set.singleton playerPos'
      }

-- Main application loop
appLoop :: App -> IO ()
appLoop app
  | currentScreen app == Exit = return ()
  | otherwise = do
      renderApp app
      c <- getChar
      newApp <- handleAppInput c app
      appLoop newApp

-- Handle input at the application level
handleAppInput :: Char -> App -> IO App
handleAppInput c app = case currentScreen app of
  MainMenu -> case c of
    'n' -> do
      initialGame <- initGame
      return app {currentScreen = Playing, gameState = Just initialGame}
    'q' -> return app {currentScreen = Exit} -- Quit the application
    _ -> return app
  Playing -> case gameState app of
    Just gs -> do
      let newGs = handleGameInput c gs
      if gameOver newGs
        then return app {currentScreen = GameOverScreen, gameState = Just newGs}
        else return app {gameState = Just newGs}
    Nothing -> return app -- Should not happen (or indicate an error)
  GameOverScreen -> case c of
    'r' -> do
      initialGame <- initGame
      return app {currentScreen = Playing, gameState = Just initialGame}
    'q' -> return app {currentScreen = MainMenu, gameState = Nothing} -- Go back to main menu
    _ -> return app -- Wait for input
  CharacterCreation -> return app -- TODO (Placeholder for future functionality)

-- Existing game input handling (pure)
handleGameInput :: Char -> GameState -> GameState
handleGameInput c state =
  let newState = case c of
        'w' -> movePlayer (Position (x (playerPos state)) (y (playerPos state) - 1)) state
        'a' -> movePlayer (Position (x (playerPos state) - 1) (y (playerPos state))) state
        's' -> movePlayer (Position (x (playerPos state)) (y (playerPos state) + 1)) state
        'd' -> movePlayer (Position (x (playerPos state) + 1) (y (playerPos state))) state
        'q' -> state {gameOver = True} -- This will trigger the transition to GameOverScreen in handleAppInput
        _ -> state
   in updateVisibility newState

-- Update visibility (Fog of War)
updateVisibility :: GameState -> GameState
updateVisibility state =
  let px = x (playerPos state)
      py = y (playerPos state)
      radius = 5 -- Visibility radius
      newVisible =
        [ Position (px + dx) (py + dy)
          | dx <- [-radius .. radius],
            dy <- [-radius .. radius],
            (dx * dx + dy * dy) <= (radius * radius)
        ]

      newExplored = foldr Set.insert (exploredTiles state) newVisible
   in state {exploredTiles = newExplored}

-- Entry point
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False

  initialApp <- initApp
  appLoop initialApp

  putStrLn $ "\nGoodbye!" ++ showCursor
