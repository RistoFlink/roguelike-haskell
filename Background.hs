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
getBackgroundBoosts AnimalWhisperer = BackgroundBoosts [Wis, Cha] True
getBackgroundBoosts Artisan = BackgroundBoosts [Str, Int] True
getBackgroundBoosts Artist = BackgroundBoosts [Dex, Cha] True
getBackgroundBoosts Barkeep = BackgroundBoosts [Con, Cha] True
getBackgroundBoosts Barrister = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts BountyHunter = BackgroundBoosts [Str, Wis] True
getBackgroundBoosts Charlatan = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts Criminal = BackgroundBoosts [Dex, Int] True
getBackgroundBoosts Detective = BackgroundBoosts [Int, Wis] True
getBackgroundBoosts Emissary = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts Entertainer = BackgroundBoosts [Dex, Cha] True
getBackgroundBoosts Farmhand = BackgroundBoosts [Con, Wis] True
getBackgroundBoosts FieldMedic = BackgroundBoosts [Con, Wis] True
getBackgroundBoosts FortuneTeller = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts Gambler = BackgroundBoosts [Dex, Cha] True
getBackgroundBoosts Gladiator = BackgroundBoosts [Str, Cha] True
getBackgroundBoosts Guard = BackgroundBoosts [Str, Cha] True
getBackgroundBoosts Herbalist = BackgroundBoosts [Con, Wis] True
getBackgroundBoosts Hermit = BackgroundBoosts [Con, Int] True
getBackgroundBoosts Hunter = BackgroundBoosts [Dex, Wis] True
getBackgroundBoosts Laborer = BackgroundBoosts [Str, Con] True
getBackgroundBoosts MartialDisciple = BackgroundBoosts [Str, Dex] True
getBackgroundBoosts Merchant = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts Miner = BackgroundBoosts [Str, Wis] True
getBackgroundBoosts Noble = BackgroundBoosts [Int, Cha] True
getBackgroundBoosts Nomad = BackgroundBoosts [Con, Wis] True
getBackgroundBoosts Prisoner = BackgroundBoosts [Str, Con] True
getBackgroundBoosts Sailor = BackgroundBoosts [Str, Dex] True
getBackgroundBoosts Scholar = BackgroundBoosts [Int, Wis] True
getBackgroundBoosts Scout = BackgroundBoosts [Dex, Wis] True
getBackgroundBoosts StreetUrchin = BackgroundBoosts [Dex, Con] True
getBackgroundBoosts Tinker = BackgroundBoosts [Dex, Int] True
getBackgroundBoosts Warrior = BackgroundBoosts [Str, Con] True
