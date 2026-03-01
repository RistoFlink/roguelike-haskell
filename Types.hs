module Types
  ( Position (..),
    Tile (..),
    Entity (..),
    Monster (..),
    Item (..),
    GameState (..),
    Screen (..),
    App (..),
    Ancestry (..),
    Background (..),
    Class (..),
    Size (..),
    CreationState (..),
    CreationStep (..),
    dungeonWidth,
    dungeonHeight,
  )
where

import Data.Set qualified as Set
import Stats
import System.Random (StdGen)

-- Position in the dungeon
data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

-- Tile types in the dungeon
data Tile
  = Floor
  | Wall
  | Door
  deriving (Eq, Show)

-- Type of entities (monsters, items, etc.)
data Entity
  = Player
  | Enemy Ancestry
  | Potion
  | Sword
  deriving (Eq, Show)

-- Monster with position, type, and health
data Monster = Monster
  { mPos :: Position,
    mType :: Entity,
    mStats :: Stats,
    mHealth :: Int
  }
  deriving (Eq, Show)

-- Item with position and type
data Item = Item
  { iPos :: Position,
    iType :: Entity
  }
  deriving (Eq, Show)

-- Complete game state
data GameState = GameState
  { playerPos :: Position,
    playerHealth :: Int,
    playerMaxHealth :: Int,
    playerAttack :: Int,
    dungeon :: [[Tile]],
    monsters :: [Monster],
    items :: [Item],
    message :: String,
    gameOver :: Bool,
    rng :: StdGen,
    exploredTiles :: Set.Set Position
  }
  deriving (Show)

-- Screen State
data Screen
  = MainMenu
  | CharacterCreation
  | Playing
  | GameOverScreen
  | Exit
  deriving (Show, Eq)

data CreationStep
  = PickAncestry
  | PickAncestryFreeBoost
  | PickBackground
  | PickBackgroundChoice
  | PickBackgroundFreeBoost
  | PickClass
  | PickKeyAbility
  | PickFinalBoosts [Ability]
  deriving (Show, Eq)

data CreationState = CreationState
  { currentStep :: CreationStep,
    chosenAncestry :: Maybe Ancestry,
    chosenBackground :: Maybe Background,
    chosenClass :: Maybe Class,
    currentStats :: Stats
  }
  deriving (Show, Eq)

-- Top-level Application State
data App = App
  { currentScreen :: Screen,
    gameState :: Maybe GameState,
    creation :: Maybe CreationState
  }
  deriving (Show)

-- Ancestry enums
data Ancestry = Dwarf | Elf | Gnome | Goblin | Halfling | Human | Orc
  deriving (Show, Eq)

data Size = Tiny | Small | Medium | Large | Huge
  deriving (Show, Eq)

-- Background enums
data Background
  = Acolyte
  | Acrobat
  | AnimalWhisperer
  | Artisan
  | Artist
  | Barkeep
  | Barrister
  | BountyHunter
  | Charlatan
  | Criminal
  | Detective
  | Emissary
  | Entertainer
  | Farmhand
  | FieldMedic
  | FortuneTeller
  | Gambler
  | Gladiator
  | Guard
  | Herbalist
  | Hermit
  | Hunter
  | Laborer
  | MartialDisciple
  | Merchant
  | Miner
  | Noble
  | Nomad
  | Prisoner
  | Sailor
  | Scholar
  | Scout
  | StreetUrchin
  | Tinker
  | Warrior
  deriving (Show, Eq)

-- Class enums
data Class
  = Alchemist
  | Barbarian
  | Bard
  | Champion
  | Cleric
  | Druid
  | Fighter
  | Monk
  | Ranger
  | Rogue
  | Sorcerer
  | Wizard
  deriving (Show, Eq)

-- Constants
dungeonWidth :: Int
dungeonWidth = 50

dungeonHeight :: Int
dungeonHeight = 20
