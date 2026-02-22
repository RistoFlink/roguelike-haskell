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
