module Ancestry where

import Stats
import Types

-- Hit points
getAncestryHP :: Ancestry -> Int
getAncestryHP Dwarf = 10
getAncestryHP Elf = 6
getAncestryHP Gnome = 8
getAncestryHP Goblin = 6
getAncestryHP Halfling = 6
getAncestryHP Human = 8
getAncestryHP Orc = 10

-- Base speed TODO: what does this actually do?
getAncestrySpeed :: Ancestry -> Int
getAncestrySpeed Dwarf = 20
getAncestrySpeed Elf = 30
getAncestrySpeed Gnome = 25
getAncestrySpeed Goblin = 25
getAncestrySpeed Halfling = 25
getAncestrySpeed Human = 25
getAncestrySpeed Orc = 25

getAncestrySize :: Ancestry -> Size
getAncestrySize Dwarf = Medium
getAncestrySize Elf = Medium
getAncestrySize Gnome = Small
getAncestrySize Goblin = Small
getAncestrySize Halfling = Small
getAncestrySize Human = Medium
getAncestrySize Orc = Medium

-- Apply the Boosts and Flaws for each Ancestry
applyAncestryStats :: Ancestry -> Stats -> Stats
applyAncestryStats Dwarf s =
  s {con = con s + 2, wis = wis s + 2, cha = cha s - 2}
applyAncestryStats Elf s =
  s {dex = dex s + 2, int = int s + 2, con = con s - 2}
applyAncestryStats Gnome s =
  s {con = con s + 2, cha = cha s + 2, str = str s - 2}
applyAncestryStats Goblin s =
  s {dex = dex s + 2, cha = cha s + 2, wis = wis s - 2}
applyAncestryStats Halfling s =
  s {dex = dex s + 2, wis = wis s + 2, str = str s - 2}
applyAncestryStats Human s = s
applyAncestryStats Orc s =
  s {str = str s + 2, con = con s + 2, int = int s - 2}
