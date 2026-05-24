module Random where

import Ancestry
import Background
import Class
import Data.List (delete)
import Stats
import System.Random (randomRIO)
import Types

rollDice :: Int -> Int -> IO Int
rollDice n sides = sum <$> mapM (\_ -> randomRIO (1, sides)) [1 .. n]

randomizeCharacter :: IO (Ancestry, Class, Stats)
randomizeCharacter = do
  anc <- (playableAncestries !!) <$> randomRIO (0, length playableAncestries - 1)

  ancBoost <- toEnum <$> randomRIO (0, 5)

  let allBgs = [minBound .. maxBound] :: [Background]
  bg <- (allBgs !!) <$> randomRIO (0, length allBgs - 1)
  let bgBoosts = getBackgroundBoosts bg
  bgChoice <- (choices bgBoosts !!) <$> randomRIO (0, length (choices bgBoosts) - 1)

  bgFree <- toEnum <$> randomRIO (0, 5)

  let allCls = [minBound .. maxBound] :: [Class]
  cls <- (allCls !!) <$> randomRIO (0, length allCls - 1)
  let keyOpts = getKeyAbilityOptions cls
  keyAbil <- (keyOpts !!) <$> randomRIO (0, length keyOpts - 1)

  finalBoosts <- pickUnique 4 [Str .. Cha]

  let stats = applyAncestryStats anc baseStats
      stats' = foldr applyBoost stats ([ancBoost, bgChoice, bgFree, keyAbil] ++ finalBoosts)

  return (anc, cls, stats')

pickUnique :: (Eq a) => Int -> [a] -> IO [a]
pickUnique 0 _ = return []
pickUnique _ [] = return []
pickUnique n xs = do
  idx <- randomRIO (0, length xs - 1)
  let picked = xs !! idx
  rest <- pickUnique (n - 1) (delete picked xs)
  return (picked : rest)
