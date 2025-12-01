module Entity
  ( spawnMonsters,
    spawnItems,
    moveMonsters,
    pickupItem,
  )
where

import Data.List (delete)
import Dungeon (findEmptySpace, getTile)
import System.Random (StdGen, randomR)
import Types

-- Spawn a number of random monsters in the dungeon
spawnMonsters :: [[Tile]] -> Int -> StdGen -> ([Monster], StdGen)
spawnMonsters _ 0 gen = ([], gen)
spawnMonsters dungeon n gen =
  let (pos, gen1) = findEmptySpace dungeon gen
      (mType, gen2) = randomR (0, 1) gen1 :: (Int, StdGen)
      monster =
        Monster
          pos
          (if mType == 0 then Goblin else Orc)
          (if mType == 0 then 5 else 8)
      (rest, gen3) = spawnMonsters dungeon (n - 1) gen2
   in (monster : rest, gen3)

-- Spawn a number of random items in the dungeon
spawnItems :: [[Tile]] -> Int -> StdGen -> ([Item], StdGen)
spawnItems _ 0 gen = ([], gen)
spawnItems dungeon n gen =
  let (pos, gen1) = findEmptySpace dungeon gen
      (iType, gen2) = randomR (0, 1) gen1 :: (Int, StdGen)
      item = Item pos (if iType == 0 then Potion else Sword)
      (rest, gen3) = spawnItems dungeon (n - 1) gen2
   in (item : rest, gen3)

-- Move all monsters and handle combat
moveMonsters :: GameState -> GameState
moveMonsters state =
  let movedMonsters = map (moveMonster state) (monsters state)
      (playerDamage, msgs) = foldl calcDamage (0, []) movedMonsters
      newHealth = playerHealth state - playerDamage
      combatMsg =
        if playerDamage > 0
          then unwords msgs ++ " (-" ++ show playerDamage ++ " HP)"
          else ""
      isGameOver = newHealth <= 0
   in state
        { monsters = movedMonsters,
          playerHealth = newHealth,
          message = if isGameOver then "You died! Press Q to quit." else combatMsg,
          gameOver = isGameOver
        }
  where
    calcDamage (dmg, msgs) monster =
      if mPos monster == playerPos state
        then (dmg + 2, msgs ++ ["Monster attacks!"])
        else (dmg, msgs)

-- Move a single monster toward the player
moveMonster :: GameState -> Monster -> Monster
moveMonster state monster =
  let Position mx my = mPos monster
      Position px py = playerPos state
      dx = signum (px - mx)
      dy = signum (py - my)
      newPos = Position (mx + dx) (my + dy)
   in if getTile (dungeon state) newPos == Floor
        && not (any (\m -> mPos m == newPos) (monsters state))
        && newPos /= playerPos state
        then monster {mPos = newPos}
        else monster

-- Pick up an item and apply its effect
pickupItem :: Item -> GameState -> GameState
pickupItem item state =
  case iType item of
    Potion ->
      state
        { items = delete item (items state),
          playerHealth = min (playerMaxHealth state) (playerHealth state + 10),
          message = "You drank a healing potion! (Recovered 10 HP)"
        }
    Sword ->
      state
        { items = delete item (items state),
          playerAttack = playerAttack state + 2,
          message = "You found a rusty sword! (+2 attack)"
        }
    _ -> state
