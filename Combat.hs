module Combat
  ( attackMonster,
    movePlayer,
  )
where

import Data.List (delete, find)
import Data.Maybe (fromJust, isJust)
import Dungeon (getTile)
import Entity (moveMonsters, pickupItem)
import Stats
import System.Random (randomR)
import Types

-- Move the player to a new position and handle interactions
movePlayer :: Position -> GameState -> GameState
movePlayer newPos state
  | not (isPassable (getTile (dungeon state) newPos)) =
      state {message = "You bump into a wall."}
  | isJust monsterHere =
      attackMonster (fromJust monsterHere) state
  | isJust itemHere =
      pickupItem (fromJust itemHere) state {playerPos = newPos}
  | otherwise =
      let baseMsg = case targetTile of
            StairsUp -> "You see stairs going up."
            StairsDown -> "You see stairs going down."
            _ -> ""
       in moveMonsters $ state {playerPos = newPos, message = baseMsg}
  where
    monsterHere = find (\m -> mPos m == newPos) (monsters state)
    itemHere = find (\i -> iPos i == newPos) (items state)
    targetTile = getTile (dungeon state) newPos

    -- Helper to define what tiles the player can walk on
    isPassable t = t == Floor || t == StairsUp || t == StairsDown

-- Attack a monster and update the game state
attackMonster :: Monster -> GameState -> GameState
attackMonster monster state =
  let (attackRoll, gen1) = randomR (1, 20) (rng state)
      pStats = playerStats state
      cStats = playerCombatStats state
      mStats' = mStats monster

      attackBonus = meleeAttack cStats
      totalAttack = attackRoll + attackBonus
      targetArmorClass = getArmorClass mStats'

      -- P2Fe crit system = N20 or 10+ over AC
      isCrit = attackRoll == 20 || totalAttack >= targetArmorClass + 10
      isHit = attackRoll /= 1 && (totalAttack >= targetArmorClass || isCrit)

      (damageRoll, finalGenerator) = randomR (1, 6) gen1
      baseDamage = damageRoll + getMeleeDamageBonus pStats
      finalDamage = if isCrit then baseDamage * 2 else baseDamage

      -- Miss -> damage is 0
      appliedDamage = if isHit then finalDamage else 0

      newHealth = mHealth monster - appliedDamage
      newMonsters =
        if newHealth <= 0
          then delete monster (monsters state)
          else map (updateMonster newHealth) (monsters state)

      combatMessage
        | not isHit = "You missed the " ++ show (mType monster) ++ "!"
        | isCrit = "Critical hit! You deal " ++ show appliedDamage ++ " damage to the " ++ show (mType monster) ++ "!"
        | otherwise =
            "You hit the "
              ++ show (mType monster)
              ++ " for "
              ++ show appliedDamage
              ++ " damage!"

      stateAfterAttack =
        state
          { monsters = newMonsters,
            message = combatMessage,
            rng = finalGenerator
          }
   in moveMonsters stateAfterAttack
  where
    updateMonster hp m =
      if m == monster
        then m {mHealth = hp}
        else m
