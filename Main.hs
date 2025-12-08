module Main where

import Combat (movePlayer)
import Dungeon (findEmptySpace, generateDungeon)
import Entity (spawnItems, spawnMonsters)
import Rendering (renderGame, showCursor)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, stdout)
import System.Random (getStdGen)
import Types

-- Initialize the game with a randomized world
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
        message = "Welcome! Use WASD to move, Q to quit.",
        gameOver = False,
        rng = gen4
      }

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop state = do
  renderGame state
  if gameOver state
    then putStrLn $ "\nThanks for playing!" ++ showCursor
    else do
      c <- getChar
      let newState = handleInput c state
      if gameOver newState 
        then do
             renderGame newState
             putStrLn $ "\nThanks for playing!" ++ showCursor
        else gameLoop newState

-- Player input handling
handleInput :: Char -> GameState -> GameState
handleInput c state = case c of
  'w' -> movePlayer (Position (x (playerPos state)) (y (playerPos state) - 1)) state
  'a' -> movePlayer (Position (x (playerPos state) - 1) (y (playerPos state))) state
  's' -> movePlayer (Position (x (playerPos state)) (y (playerPos state) + 1)) state
  'd' -> movePlayer (Position (x (playerPos state) + 1) (y (playerPos state))) state
  'q' -> state {gameOver = True}
  _ -> state

-- Entry point
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  initialState <- initGame
  gameLoop initialState
