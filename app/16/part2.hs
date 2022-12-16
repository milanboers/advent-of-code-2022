{-# LANGUAGE TupleSections #-}

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Valve = (String, Int, Set String)

type Network = Map String Valve

-- (flow at end, current flow, current valve name, closed valves)
type ActorState = (Int, Int, String, Set String)

-- (flow at end, current flow, current valve name, closed valves)
data State = State Int Int String String (Set String) deriving (Eq, Show, Ord)

actorNextStates :: Network -> Int -> ActorState -> Set ActorState
actorNextStates network timeLeft (endFlow, flow, valveName, closed) =
  Set.unions
    [ stayPutStates,
      tunnelMoveStates,
      openValveStates
    ]
  where
    (_, valveFlow, tunnels) = network ! valveName

    stayPutStates = Set.singleton (endFlow, flow, valveName, closed)
    tunnelMoveStates = Set.map (endFlow,flow,,closed) tunnels
    openValveStates =
      if valveName `Set.member` closed
        then Set.empty
        else
          Set.singleton
            (endFlow + valveFlow * (timeLeft - 1), flow + valveFlow, valveName, Set.insert valveName closed)

nextStates :: Network -> Int -> State -> Set State
nextStates _ 0 _ = Set.empty
nextStates network timeLeft (State endFlow flow valveName eleValveName closed) =
  Set.map (\(n, (fe, f, e, c)) -> State fe f n e c) eleTurn
  where
    -- first my actions, elephant stays put
    myTurn = actorNextStates network timeLeft (endFlow, flow, valveName, closed)
    -- then the elephant's actions
    eleTurn =
      Set.unions
        . Set.map (\(fe, f, n, c) -> Set.map (n,) (actorNextStates network timeLeft (fe, f, eleValveName, c)))
        $ myTurn

insertSetMap :: Ord a => Ord b => Map b (Set a) -> b -> a -> Map b (Set a)
insertSetMap m k v = Map.insert k newValue m
  where
    newValue = case Map.lookup k m of
      Just oldValue -> Set.insert v oldValue
      Nothing -> Set.singleton v

values :: Ord b => Map a b -> Set b
values = foldl (flip Set.insert) Set.empty

prune :: Set State -> Set State
prune states = values grouped'
  where
    -- Group by valve name, keep only one with highest total flow
    toKey n en = concat $ sort [n, en] -- might be incorrect but works
    kvs = Set.map (\s@(State _ f n en _) -> ((toKey n en, f), s)) states
    grouped = foldl (\m (k, v) -> insertSetMap m k v) Map.empty kvs
    grouped' = Map.map Set.findMax grouped

steps :: Network -> Int -> Set State -> Set State
steps network timeLeft states =
  if Set.null pruned
    then states
    else steps network (timeLeft - 1) pruned
  where
    -- intermediate prune for ugly memory optimization
    nextStates' = Set.unions . Set.map (prune . nextStates network timeLeft) $ states
    pruned = prune nextStates'

findAnswer :: Network -> Int
findAnswer network = maximum . Set.map (\(State endFlow _ _ _ _) -> endFlow) $ finalStates
  where
    finalStates = steps network 26 $ Set.singleton (State 0 0 "AA" "AA" Set.empty)

parseValve :: String -> Valve
parseValve xs = (name, flowRate, tunnels)
  where
    name = head . splitOn " has" . last . splitOn "Valve " $ xs
    flowRate = read . head . splitOn ";" . last . splitOn "rate=" $ xs
    tunnels = Set.fromList . splitOn ", " . dropWhile (`elem` ['s', ' ']) . last . splitOn "to valve" $ xs

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let valves = map parseValve input
  let network = Map.fromList . map (\v@(n, _, _) -> (n, v)) $ valves
  print $ findAnswer network
