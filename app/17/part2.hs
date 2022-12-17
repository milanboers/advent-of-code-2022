import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Gas = L | R deriving (Eq, Show)

data Rock = Flat | Plus | Corner | Straight | Square deriving (Eq, Show, Ord)

type State = (Set (Int, Int), [Gas], Int, Int)

rockShape :: Rock -> Set (Int, Int)
rockShape Flat = Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]
rockShape Plus = Set.fromList [(1, 0), (0, 1), (1, 1), (1, 2), (2, 1)]
rockShape Corner = Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
rockShape Straight = Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)]
rockShape Square = Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

towerHeight :: Set (Int, Int) -> Int
towerHeight m | Set.null m = 0
towerHeight m = (+ 1) . Set.findMax . Set.map snd $ m

move :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
move (dx, dy) = Set.map (\(x, y) -> (x + dx, y + dy))

gasDelta :: Gas -> (Int, Int)
gasDelta L = (-1, 0)
gasDelta R = (1, 0)

outOfBounds :: Set (Int, Int) -> Bool
outOfBounds = any (\(x, y) -> x < 0 || y < 0 || x > 6)

impossiblePosition :: Set (Int, Int) -> Set (Int, Int) -> Bool
impossiblePosition m rockC = outOfBounds rockC || (not . Set.null) (Set.intersection m rockC)

pushRock :: Set (Int, Int) -> Gas -> Set (Int, Int) -> Set (Int, Int)
pushRock m gas rockC = if impossiblePosition m movedRockC then rockC else movedRockC
  where
    gd = gasDelta gas
    movedRockC = move gd rockC

fallRock :: Set (Int, Int) -> Set (Int, Int) -> (Set (Int, Int), Bool)
fallRock m rockC = if impossiblePosition m movedRockC then (rockC, True) else (movedRockC, False)
  where
    movedRockC = move (0, -1) rockC

dropRockSteps :: State -> Set (Int, Int) -> State
dropRockSteps (m, gas : gasRest, cycleLength, cycleI) rockC = case fallRock m pushed of
  (newRockC, True) -> (Set.union m newRockC, gasRest, cycleLength, newCycleI)
  (newRockC, False) -> dropRockSteps (m, gasRest, cycleLength, newCycleI) newRockC
  where
    pushed = pushRock m gas rockC
    newCycleI = (cycleI + 1) `mod` cycleLength
dropRockSteps (_, _, _, _) _ = error "No gas. Impossible"

dropRock :: State -> Rock -> State
dropRock (m, gas, cycleLength, cycleI) rock = dropRockSteps (m, gas, cycleLength, cycleI) rockStartC
  where
    height = towerHeight m
    rockStartC = move (2, height + 3) $ rockShape rock

-- (gas i at cycle, rock at cycle, drops in cycle, added height in cycle, drops when detected, shape when detected)
detectCycle :: State -> [Rock] -> (Int, Rock, Int, Int, Int, Set (Int, Int))
detectCycle = detectCycle' Map.empty 0
  where
    detectCycle' :: Map (Int, Rock) (Int, Int) -> Int -> State -> [Rock] -> (Int, Rock, Int, Int, Int, Set (Int, Int))
    detectCycle' seen drops state (rock : restRocks) = case dropRock state rock of
      (m, _, _, ci)
        | (ci, rock) `Map.member` seen && drops > dropsInCycle * 2 ->
            (ci, head restRocks, dropsInCycle, towerHeight m - snd (seen ! (ci, rock)), drops, m)
        where
          dropsInCycle = drops - fst (seen ! (ci, rock))
      newState@(m, _, _, ci) ->
        detectCycle' (Map.insert (ci, rock) (drops, towerHeight m) seen) (drops + 1) newState restRocks
    detectCycle' _ _ _ [] = error "Impossible"

findAnswer :: [Gas] -> Int -> Int
findAnswer gas gasLength = heightWithoutCycles + heightOfAllCycles
  where
    need = 1000000000000

    shapes = cycle [Flat, Plus, Corner, Straight, Square]
    initialState = (Set.empty, gas, gasLength, 0)
    ( ciAfterCycle,
      cycleRock,
      dropsInCycle,
      cycleHeight,
      dropsWhenDetected,
      shapeWhenDetected
      ) = detectCycle initialState shapes

    -- how many more drops do we need
    need' = need - dropsWhenDetected
    (needCycles, needSingleDrops) = divMod need' dropsInCycle
    heightOfAllCycles = needCycles * cycleHeight

    gasAfterCycle = drop ciAfterCycle gas
    shapesAfterCycle = dropWhile (/= cycleRock) shapes
    states =
      scanl
        (\(m, g, cl, ci) rock -> dropRock (m, g, cl, ci) rock)
        (shapeWhenDetected, gasAfterCycle, gasLength, ciAfterCycle)
        shapesAfterCycle
    (shapeWithoutCycles, _, _, _) = last . take needSingleDrops $ states
    heightWithoutCycles = towerHeight shapeWithoutCycles

parseGas :: Char -> Gas
parseGas '<' = L
parseGas '>' = R
parseGas _ = error "Invalid gas"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let gas = map parseGas . head $ input
  print $ findAnswer (cycle gas) (length gas)
