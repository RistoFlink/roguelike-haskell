module Background where

import Stats
import Types

displayBackground :: Background -> String
displayBackground AnimalWhisperer = "Animal Whisperer"
displayBackground BountyHunter = "Bounty Hunter"
displayBackground FieldMedic = "Field Medic"
displayBackground FortuneTeller = "Fortune Teller"
displayBackground MartialDisciple = "Martial Disciple"
displayBackground StreetUrchin = "Street Urchin"
displayBackground bg = show bg

data BackgroundBoosts = BackgroundBoosts
  { choices :: [Ability],
    hasFree :: Bool
  }

getBackgroundBoosts :: Background -> BackgroundBoosts
getBackgroundBoosts Acolyte = BackgroundBoosts [Int, Wis] True
getBackgroundBoosts Acrobat = BackgroundBoosts [Str, Dex] True
