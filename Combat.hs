module Combat
  ( attackMonster,
    movePlayer,
  )
where

import Data.List (delete, find)
import Data.Maybe (fromJust, isJust)
import Dungeon (getTile)
import Entity (moveMonsters, pickupItem)
import Types

-- Move the player to a new position and handle interactions
movePlayer :: Position -> GameState -> GameState
movePlayer newPos state
  | getTile (dungeon state) newPos /= Floor =
      state {message = "You bump into a wall."}
  | isJust monsterHere =
      attackMonster (fromJust monsterHere) state
  | isJust itemHere =
      pickupItem (fromJust itemHere) state {playerPos = newPos}
  | otherwise =
      moveMonsters $ state {playerPos = newPos, message = ""}
  where
    monsterHere = find (\m -> mPos m == newPos) (monsters state)
    itemHere = find (\i -> iPos i == newPos) (items state)

-- Attack a monster and update the game state
attackMonster :: Monster -> GameState -> GameState
attackMonster monster state =
  let damage = playerAttack state
      newHealth = mHealth monster - damage
      newMonsters =
        if newHealth <= 0
          then delete monster (monsters state)
          else map (updateMonster newHealth) (monsters state)
      msg =
        if newHealth <= 0
          then "You defeated the " ++ show (mType monster) ++ "!"
          else "You hit the " ++ show (mType monster) ++ " for " ++ show damage ++ " damage!"
      stateAfterAttack = state {monsters = newMonsters, message = msg}
   in moveMonsters stateAfterAttack
  where
    updateMonster hp m =
      if m == monster
        then m {mHealth = hp}
        else m
