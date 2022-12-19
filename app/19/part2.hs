{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.Bifunctor
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Type = Ore | Clay | Obsidian | Geode deriving (Eq, Show, Ord)

type Cost = Map Type Int

type Blueprint = Map Type Cost

type Inventory = Map Type Int

-- (# robots, inventory)
type State = (Map Type Int, Inventory)

types :: Set Type
types = Set.fromList [Ore, Clay, Obsidian, Geode]

has :: Map Type Int -> Type -> Int
has inv tp = Map.findWithDefault 0 tp inv

canBuyRobot :: Blueprint -> Inventory -> Type -> Maybe Inventory
canBuyRobot bp inv typ = if any (< 0) invUpdates then Nothing else Just (Map.union invUpdates inv)
  where
    robotCosts = bp ! typ
    invUpdates = Map.mapWithKey (\k v -> has inv k - v) robotCosts

-- -> (robot built, new inventory)
robotBuildingOptions :: Blueprint -> Inventory -> Set (Type, Inventory)
robotBuildingOptions bp inv =
  Set.map (Data.Bifunctor.second fromJust)
    . Set.filter (\(_, m) -> isJust m)
    . Set.map (\t -> (t, canBuyRobot bp inv t))
    $ types

collect :: Map Type Int -> Inventory -> Inventory
collect robots inv = Map.mapWithKey (\k v -> has inv k + v) robots

addRobot :: Map Type Int -> Type -> Map Type Int
addRobot m typ = Map.insert typ (has m typ + 1) m

prune :: Set State -> Set State
prune states = toKeep'
  where
    -- Greedily go to states that have a lot of geode robots
    maxGeodeRobots = Set.findMax $ Set.map (\(robs, _) -> has robs Geode) states
    toKeep = Set.filter (\(robs, _) -> has robs Geode >= maxGeodeRobots - 1) states

    -- Greedily go to states that have a lot of obsidian robots
    maxObsRobots = Set.findMax $ Set.map (\(robs, _) -> has robs Obsidian) toKeep
    toKeep' = Set.filter (\(robs, _) -> has robs Obsidian >= maxObsRobots - 2) toKeep

minute :: Blueprint -> State -> Set State
minute bp (robots, inv) = Set.map (Data.Bifunctor.second (collect robots)) allOptions
  where
    -- Do not build. Keep same robots, same inv.
    notBuildOptionState = (robots, inv)
    -- Which robots can I buy and what will will inv be afterwards?
    buildingOptions = robotBuildingOptions bp inv
    statesForBuildingOptions = Set.map (Data.Bifunctor.first (addRobot robots)) buildingOptions
    --
    allOptions = Set.insert notBuildOptionState statesForBuildingOptions

minutes :: Blueprint -> Set State -> Int -> Set State
minutes _ states 0 = states
minutes bp states i = minutes bp nextStates (i - 1)
  where
    nextStates = prune $ Set.unions (Set.map (minute bp) states)

qualityLevel :: Blueprint -> Int
qualityLevel bp = Set.findMax . Set.map (\(_, inv) -> has inv Geode) $ endStates
  where
    startStates = Set.singleton (Map.singleton Ore 1, Map.empty)
    endStates = minutes bp startStates 32

findAnswer :: [Blueprint] -> Int
findAnswer = product . map qualityLevel . take 3

parseBlueprint :: String -> Blueprint
parseBlueprint line =
  Map.fromList
    [ (Ore, Map.singleton Ore oreCostsOre),
      (Clay, Map.singleton Ore clayCostsOre),
      (Obsidian, Map.fromList [(Ore, obsCostsOre), (Clay, obsCostsClay)]),
      (Geode, Map.fromList [(Ore, geodeCostsOre), (Obsidian, geodeCostsObs)])
    ]
  where
    [r1s, r2s, r3s, r4s, _] = splitOn "." line
    readIntAfterString s = read . head . splitOn " " . last . splitOn s

    oreCostsOre = readIntAfterString "costs " r1s
    clayCostsOre = readIntAfterString "costs " r2s
    (obsCostsOreS, r3rest) = break (== ' ') . last . splitOn "costs " $ r3s
    obsCostsOre = read obsCostsOreS
    obsCostsClay = readIntAfterString " and " r3rest
    (geodeCostsOreS, r4rest) = break (== ' ') . last . splitOn "costs " $ r4s
    geodeCostsOre = read geodeCostsOreS
    geodeCostsObs = readIntAfterString " and " r4rest

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let bps = map parseBlueprint input
  print $ findAnswer bps
