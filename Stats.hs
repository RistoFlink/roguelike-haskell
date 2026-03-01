module Stats where

data Ability = Str | Dex | Con | Int | Wis | Cha
  deriving (Show, Eq, Enum, Bounded)

data Stats = Stats
  { str :: Int,
    dex :: Int,
    con :: Int,
    int :: Int,
    wis :: Int,
    cha :: Int
  }
  deriving (Show, Eq)

getModifier :: Int -> Int
getModifier n = (n - 10) `div` 2

baseStats :: Stats
baseStats = Stats 10 10 10 10 10 10

applyBoost :: Ability -> Stats -> Stats
applyBoost Str s = s {str = str s + 2}
applyBoost Dex s = s {dex = dex s + 2}
applyBoost Con s = s {con = con s + 2}
applyBoost Int s = s {int = int s + 2}
applyBoost Wis s = s {wis = wis s + 2}
applyBoost Cha s = s {cha = cha s + 2}

-- Armor class and attack bonuses
getArmorClass :: Stats -> Int
getArmorClass s = 10 + getModifier (dex s)

getMeleeAttackBonus :: Stats -> Int
getMeleeAttackBonus s = getModifier (str s)

getMeleeDamageBonus :: Stats -> Int
getMeleeDamageBonus s = getModifier (str s)
