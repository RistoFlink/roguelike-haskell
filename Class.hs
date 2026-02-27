module Class where

import Stats
import Types

-- HP per level
getClassHP :: Class -> Int
getClassHP Alchemist = 8
getClassHP Barbarian = 12
getClassHP Bard = 8
getClassHP Champion = 10
getClassHP Cleric = 8
getClassHP Druid = 8
getClassHP Fighter = 10
getClassHP Monk = 10
getClassHP Ranger = 10
getClassHP Rogue = 8
getClassHP Sorcerer = 6
getClassHP Wizard = 6

-- Key Ability
getKeyAbilityOptions :: Class -> [Ability]
getKeyAbilityOptions Alchemist = [Int]
getKeyAbilityOptions Barbarian = [Str]
getKeyAbilityOptions Bard = [Cha]
getKeyAbilityOptions Champion = [Str, Dex]
getKeyAbilityOptions Cleric = [Wis]
getKeyAbilityOptions Druid = [Wis]
getKeyAbilityOptions Fighter = [Str, Dex]
getKeyAbilityOptions Monk = [Str, Dex]
getKeyAbilityOptions Ranger = [Str, Dex]
getKeyAbilityOptions Rogue = [Str, Dex, Int, Cha]
getKeyAbilityOptions Sorcerer = [Cha]
getKeyAbilityOptions Wizard = [Int]
