module Monsters (createMonster) where

import Stats
import Types

-- Central monster factory
createMonster :: Entity -> Position -> Monster
createMonster Goblin pos =
  Monster pos Goblin (baseStats {str = 10, dex = 14, con = 10}) 6
createMonster Orc pos =
  Monster pos Orc (baseStats {str = 14, dex = 12, con = 12}) 15
createMonster _ pos =
  Monster pos Potion baseStats 0
