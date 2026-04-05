module Main where

import Ancestry
import Background (BackgroundBoosts (..), displayBackground, getBackgroundBoosts)
import Class (getKeyAbilityOptions)
import Combat (movePlayer)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Dungeon (findEmptySpace, generateDungeon)
import Entity (spawnItems, spawnMonsters)
import Rendering (renderApp, showCursor)
import Stats (Ability (..), applyBoost, baseStats)
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, stdout)
import System.Random (getStdGen)
import Types

-- Initialize the top-level application
initApp :: IO App
initApp = return $ App {currentScreen = MainMenu, gameState = Nothing, creation = Nothing}

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
    'n' ->
      return
        app
          { currentScreen = CharacterCreation,
            creation =
              Just $
                CreationState
                  { currentStep = PickAncestry,
                    chosenAncestry = Nothing,
                    chosenBackground = Nothing,
                    chosenClass = Nothing,
                    currentStats = baseStats,
                    selectedIndex = 0,
                    currentPage = 0,
                    selectedIndices = []
                  }
          }
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
  CharacterCreation -> case creation app of
    Just cs -> handleCreationInput c cs app
    Nothing -> return app
  Exit -> return app

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

-- Handle the choices in character creation
handleCreationInput :: Char -> CreationState -> App -> IO App
handleCreationInput c cs app = case currentStep cs of
  PickAncestry -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' -> return app {creation = Just cs {selectedIndex = min (length playableAncestries - 1) (selectedIndex cs + 1)}}
    '\n' ->
      let selected = playableAncestries !! selectedIndex cs
       in return app {creation = Just (selectAncestry selected cs {selectedIndex = 0})}
    _ -> return app
  PickAncestryFreeBoost -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' -> return app {creation = Just cs {selectedIndex = min 5 (selectedIndex cs + 1)}}
    '\n' ->
      let allAbilities = [Str .. Cha]
          selectedAbil = allAbilities !! selectedIndex cs
       in return app {creation = Just (selectFreeBoost selectedAbil cs {selectedIndex = 0})}
    _ -> return app
  PickBackground -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' ->
      let itemsOnPage = min 10 (length allBackgrounds - (currentPage cs * 10))
       in return app {creation = Just cs {selectedIndex = min (itemsOnPage - 1) (selectedIndex cs + 1)}}
    'a' -> return app {creation = Just cs {currentPage = max 0 (currentPage cs - 1), selectedIndex = 0}}
    'd' ->
      let maxPage = (length allBackgrounds - 1) `div` 10
       in return app {creation = Just cs {currentPage = min maxPage (currentPage cs + 1), selectedIndex = 0}}
    '\n' ->
      let absoluteIndex = (currentPage cs * 10) + selectedIndex cs
          selectedBg = allBackgrounds !! absoluteIndex
       in return app {creation = Just (selectBackground selectedBg cs {selectedIndex = 0, currentPage = 0})}
    _ -> return app
  PickBackgroundChoice -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' ->
      let currentBg = fromJust (chosenBackground cs)
          options = choices (getBackgroundBoosts currentBg)
       in return app {creation = Just cs {selectedIndex = min (length options - 1) (selectedIndex cs + 1)}}
    '\n' ->
      let currentBg = fromJust (chosenBackground cs)
          options = choices (getBackgroundBoosts currentBg)
          selectedAbil = options !! selectedIndex cs
       in return app {creation = Just (selectBackgroundChoice selectedAbil cs {selectedIndex = 0})}
    _ -> return app
  PickBackgroundFreeBoost -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' -> return app {creation = Just cs {selectedIndex = min 5 (selectedIndex cs + 1)}}
    '\n' ->
      let allAbilities = [Str .. Cha]
          selectedAbil = allAbilities !! selectedIndex cs
       in return app {creation = Just (selectBackgroundFree selectedAbil cs {selectedIndex = 0})}
    _ -> return app
  PickClass -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' ->
      let itemsOnPage = min 10 (length allClasses - (currentPage cs * 10))
       in return app {creation = Just cs {selectedIndex = min (itemsOnPage - 1) (selectedIndex cs + 1)}}
    'a' -> return app {creation = Just cs {currentPage = max 0 (currentPage cs - 1), selectedIndex = 0}}
    'd' ->
      let maxPage = (length allClasses - 1) `div` 10
       in return app {creation = Just cs {currentPage = min maxPage (currentPage cs + 1), selectedIndex = 0}}
    '\n' ->
      let absoluteIndex = (currentPage cs * 10) + selectedIndex cs
          selectedClass = allClasses !! absoluteIndex
       in return app {creation = Just (selectClass selectedClass cs {selectedIndex = 0, currentPage = 0})}
    _ -> return app
  PickKeyAbility -> case c of
    'w' -> return app {creation = Just cs {selectedIndex = max 0 (selectedIndex cs - 1)}}
    's' ->
      let currentCls = fromJust (chosenClass cs)
          options = getKeyAbilityOptions currentCls
       in return app {creation = Just cs {selectedIndex = min (length options - 1) (selectedIndex cs + 1)}}
    '\n' ->
      let currentCls = fromJust (chosenClass cs)
          options = getKeyAbilityOptions currentCls
          selectedAbil = options !! selectedIndex cs
       in return app {creation = Just (selectKeyAbility selectedAbil cs {selectedIndex = 0})}
    _ -> return app
  _ -> return app
  where
    selectAncestry anc state =
      state
        { chosenAncestry = Just anc,
          currentStep = PickAncestryFreeBoost,
          currentStats = applyAncestryStats anc (currentStats state)
        }
    selectFreeBoost abil state =
      state
        { currentStep = PickBackground,
          currentStats = applyBoost abil (currentStats state)
        }
    allBackgrounds = [minBound .. maxBound] :: [Background]

    selectBackground bg state =
      state
        { chosenBackground = Just bg,
          currentStep = PickBackgroundChoice
        }

    selectBackgroundChoice abil state =
      state
        { currentStep = PickBackgroundFreeBoost,
          currentStats = applyBoost abil (currentStats state)
        }

    selectBackgroundFree abil state =
      state
        { currentStep = PickClass,
          currentStats = applyBoost abil (currentStats state)
        }

    allClasses = [minBound .. maxBound] :: [Class]

    selectClass cls state =
      state
        { chosenClass = Just cls,
          currentStep = PickKeyAbility
        }

    selectKeyAbility abil state =
      state
        { currentStep = PickFinalBoosts [],
          currentStats = applyBoost abil (currentStats state)
        }

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
