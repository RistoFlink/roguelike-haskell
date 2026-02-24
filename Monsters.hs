module Monsters (createMonster) where

import Ancestry
import Stats
import Types

-- Central monster factory
createMonster :: Ancestry -> Position -> Monster
createMonster anc pos =
  Monster
    { mPos = pos,
      mType = Enemy anc, -- Wrap the ancestry in the Entity type
      mStats = applyAncestryStats anc baseStats,
      mHealth = getAncestryHP anc
    }
