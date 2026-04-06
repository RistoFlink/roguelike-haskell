module Flavor (getAncestryFlavor, getBackgroundFlavor, getClassFavor) where

import Types

getAncestryFlavor :: Ancestry -> String
getAncestryFlavor Dwarf =
  "If you want to play a character who is as hard as nails, "
    ++ "a stubborn and unrelenting adventurer, with a mix of "
    ++ "rugged toughness and deep wisdom—or at least dogged "
    ++ "conviction—you should play a dwarf."
getAncestryFlavor Elf =
  "If you want a character who is "
    ++ "magical, mystical, and mysterious, "
    ++ "you should play an elf."
getAncestryFlavor Gnome =
  "If you want a character with boundless enthusiasm "
    ++ "and an alien, fey outlook on morality and life, you should "
    ++ "play a gnome."
getAncestryFlavor Goblin =
  "If you want a character who is eccentric, enthusiastic, "
    ++ "and fun-loving, you should play a goblin."
getAncestryFlavor Halfling =
  "If you want to play a character who must contend with "
    ++ "these opposing drives toward adventure and comfort, "
    ++ "you should play a halfling."
getAncestryFlavor Human =
  "If you want a character who can be "
    ++ "just about anything, you should play a human."
getAncestryFlavor _ = "Coming soon.."

getBackgroundFlavor :: Background -> String
getBackgroundFlavor _ = "Coming soon"

getClassFlavor :: Class -> String
getClassFlavor _ = "Coming soon.."
