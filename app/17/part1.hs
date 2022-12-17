import Data.Set (Set)
import qualified Data.Set as Set

data Gas = L | R deriving (Eq, Show)

data Rock = Flat | Plus | Corner | Straight | Square deriving (Eq, Show)

type State = (Set (Int, Int), [Gas])

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
dropRockSteps (m, gas : gasRest) rockC = case fallRock m pushed of
  (newRockC, True) -> (Set.union m newRockC, gasRest)
  (newRockC, False) -> dropRockSteps (m, gasRest) newRockC
  where
    pushed = pushRock m gas rockC
dropRockSteps (_, []) _ = error "No gas. Impossible"

dropRock :: State -> Rock -> State
dropRock (m, gas) rock = dropRockSteps (m, gas) rockStartC
  where
    height = towerHeight m
    rockStartC = move (2, height + 3) $ rockShape rock

findAnswer :: [Gas] -> Int
findAnswer gas = towerHeight . fst $ endState
  where
    shapes = cycle [Flat, Plus, Corner, Straight, Square]
    states = scanl (\(m, g) rock -> dropRock (m, g) rock) (Set.empty, gas) shapes
    endState = states !! 2022

parseGas :: Char -> Gas
parseGas '<' = L
parseGas '>' = R
parseGas _ = error "Invalid gas"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let gas = cycle . map parseGas . head $ input
  print $ findAnswer gas
