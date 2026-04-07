module Rendering
  ( renderApp, -- Renamed from renderGame
    clearScreen,
    hideCursor,
    showCursor,
  )
where

import Ancestry
import Background (BackgroundBoosts (..), displayBackground, getBackgroundBoosts)
import Class (getKeyAbilityOptions)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Stats
import Types
import Utils

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
  putStr $ hideCursor ++ "\ESC[H"
  putStrLn $ bold (yellow " CHARACTER CREATION ") ++ clearRestOfLine
  putStrLn clearRestOfLine

  -- Sidebar to display stats
  let renderSidebar stats = do
        putStr "\ESC[3;60H" -- Move cursor to row 3, col 60
        putStrLn $ bold $ cyan "  CURRENT STATS  " ++ clearRestOfLine
        putStr "\ESC[4;60H"
        putStrLn $ "  Str: " ++ show (str stats) ++ clearRestOfLine
        putStr "\ESC[5;60H"
        putStrLn $ "  Dex: " ++ show (dex stats) ++ clearRestOfLine
        putStr "\ESC[6;60H"
        putStrLn $ "  Con: " ++ show (con stats) ++ clearRestOfLine
        putStr "\ESC[7;60H"
        putStrLn $ "  Int: " ++ show (int stats) ++ clearRestOfLine
        putStr "\ESC[8;60H"
        putStrLn $ "  Wis: " ++ show (wis stats) ++ clearRestOfLine
        putStr "\ESC[9;60H"
        putStrLn $ "  Cha: " ++ show (cha stats) ++ clearRestOfLine

  -- Calculate stat preview based on the highlight
  let previewStats = case currentStep cs of
        PickAncestry ->
          let highlighted = playableAncestries !! selectedIndex cs
           in applyAncestryStats highlighted (currentStats cs)
        PickAncestryFreeBoost ->
          let allAbilities = [Str .. Cha]
              highlightedAbil = allAbilities !! selectedIndex cs
           in applyBoost highlightedAbil (currentStats cs)
        PickBackgroundChoice ->
          let currentBg = fromJust (chosenBackground cs)
              options = choices (getBackgroundBoosts currentBg)
              highlightedAbil = options !! selectedIndex cs
           in applyBoost highlightedAbil (currentStats cs)
        PickBackgroundFreeBoost ->
          let allAbilities = [Str .. Cha]
              highlightedAbil = allAbilities !! selectedIndex cs
           in applyBoost highlightedAbil (currentStats cs)
        PickKeyAbility ->
          let currentCls = fromJust (chosenClass cs)
              options = getKeyAbilityOptions currentCls
              highlightedAbil = options !! selectedIndex cs
           in applyBoost highlightedAbil (currentStats cs)
        PickFinalBoosts chosenBoosts ->
          let allAbilities = [Str .. Cha]
              highlightedAbil = allAbilities !! selectedIndex cs
              baseWithChosen = foldr applyBoost (currentStats cs) chosenBoosts
           in if highlightedAbil `elem` chosenBoosts
                then baseWithChosen
                else applyBoost highlightedAbil baseWithChosen
        _ -> currentStats cs

  -- Placeholder for the info box
  putStr "\ESC[22;1H"
  putStrLn $ replicate 80 '-' ++ clearRestOfLine
  putStrLn $ " INFO: TO BE IMPLEMENTED" ++ clearRestOfLine
  putStrLn $ replicate 80 '-' ++ clearRestOfLine

  -- Move cursor back to line 3 for the list
  putStr "\ESC[3;1H"

  case currentStep cs of
    PickAncestry -> do
      putStrLn $ " Pick your Ancestry:" ++ clearRestOfLine
      mapM_ (renderAncestryChoice (selectedIndex cs)) (zip [0 ..] playableAncestries)
    PickAncestryFreeBoost -> do
      putStrLn $ " Pick your Free Ancestry Boost:" ++ clearRestOfLine
      let allAbilities = [Str .. Cha]
      mapM_ (renderStatChoice (selectedIndex cs)) (zip [0 ..] allAbilities)
    PickBackground -> do
      let allBackgrounds = [minBound .. maxBound] :: [Background]
          pageItems = take 10 . drop (currentPage cs * 10) $ allBackgrounds
          maxPage = (length allBackgrounds - 1) `div` 10
      putStrLn $ " Pick your Background (Page " ++ show (currentPage cs + 1) ++ "/" ++ show (maxPage + 1) ++ "):" ++ clearRestOfLine
      mapM_ (renderBackgroundChoice (selectedIndex cs)) (zip [0 ..] pageItems)
      putStrLn clearRestOfLine
      putStrLn $ " [a] Previous Page  [d] Next Page" ++ clearRestOfLine
    PickBackgroundChoice -> do
      putStrLn $ " Pick your Background Boost:" ++ clearRestOfLine
      let currentBg = fromJust (chosenBackground cs)
          options = choices (getBackgroundBoosts currentBg)
      mapM_ (renderStatChoice (selectedIndex cs)) (zip [0 ..] options)
    PickBackgroundFreeBoost -> do
      putStrLn $ " Pick your Free Background Boost:" ++ clearRestOfLine
      let allAbilities = [Str .. Cha]
      mapM_ (renderStatChoice (selectedIndex cs)) (zip [0 ..] allAbilities)
    PickClass -> do
      let allClasses = [minBound .. maxBound] :: [Class]
          pageItems = take 10 . drop (currentPage cs * 10) $ allClasses
          maxPage = (length allClasses - 1) `div` 10
      putStrLn $ " Pick your Class (Page " ++ show (currentPage cs + 1) ++ "/" ++ show (maxPage + 1) ++ "):"
      mapM_ (renderClassChoice (selectedIndex cs)) (zip [0 ..] pageItems)
      putStrLn ""
      putStrLn $ " [a] Previous Page [d] Next Page"
    PickKeyAbility -> do
      putStrLn $ " Pick your Class Key Ability:" ++ clearRestOfLine
      let currentCls = fromJust (chosenClass cs)
          options = getKeyAbilityOptions currentCls
      mapM_ (renderStatChoice (selectedIndex cs)) (zip [0 ..] options)
    PickFinalBoosts chosenBoosts -> do
      putStrLn $ " Final Attribute Boosts (" ++ show (length chosenBoosts) ++ "/4)" ++ clearRestOfLine
      putStrLn $ " Choose 4 different attributes to increase by +2." ++ clearRestOfLine
      let allAbilities = [Str .. Cha]
      mapM_
        ( \(idx, abil) -> do
            let isSelected = abil `elem` chosenBoosts
                isHighlighted = idx == selectedIndex cs
                prefix = if isHighlighted then bold (yellow "> ") else " "
                checkbox = if isSelected then "[x] " else "[ ] "
            putStrLn $ prefix ++ checkbox ++ show abil ++ clearRestOfLine
        )
        (zip [0 ..] allAbilities)
      putStrLn ""
      putStrLn $ (if length chosenBoosts == 4 then green else white) "[Space] Toggle selection  [Enter] Finalize Character" ++ clearRestOfLine

  renderSidebar previewStats

renderAncestryChoice :: Int -> (Int, Ancestry) -> IO ()
renderAncestryChoice currentIdx (idx, anc) =
  let highlight = if idx == currentIdx then bold (yellow "> ") else " "
   in putStrLn $ highlight ++ show anc ++ clearRestOfLine

renderBackgroundChoice :: Int -> (Int, Background) -> IO ()
renderBackgroundChoice currentIdx (idx, bg) =
  let highlight = if idx == currentIdx then bold (yellow "> ") else " "
   in putStrLn $ highlight ++ displayBackground bg ++ clearRestOfLine

renderClassChoice :: Int -> (Int, Class) -> IO ()
renderClassChoice currentIdx (idx, cls) =
  let highlight = if idx == currentIdx then bold (yellow "> ") else " "
   in putStrLn $ highlight ++ show cls

renderStatChoice :: Int -> (Int, Ability) -> IO ()
renderStatChoice currentIdx (idx, abil) =
  let highlight = if idx == currentIdx then bold (yellow "> ") else " "
   in putStrLn $ highlight ++ show abil ++ clearRestOfLine

-- Render the dungeon simulation (Existing logic)
renderGame :: GameState -> IO ()
renderGame state = do
  let cStats = playerCombatStats state
      header =
        bold "HP: "
          ++ (if playerHealth state < 5 then brightRed else green)
            (show (playerHealth state) ++ "/" ++ show (maxHP cStats))
          ++ bold " | ATK: "
          ++ cyan (show (meleeAttack cStats))
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
