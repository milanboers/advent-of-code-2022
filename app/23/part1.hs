{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (round)

type Elf = (Int, Int)

type Elves = Set Elf

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Eq, Show)

directionDelta :: Direction -> (Int, Int)
directionDelta North = (0, -1)
directionDelta NorthEast = (1, -1)
directionDelta East = (1, 0)
directionDelta SouthEast = (1, 1)
directionDelta South = (0, 1)
directionDelta SouthWest = (-1, 1)
directionDelta West = (-1, 0)
directionDelta NorthWest = (-1, -1)

applyDelta :: (Int, Int) -> (Int, Int) -> (Int, Int)
applyDelta (dx, dy) (x, y) = (dx + x, dy + y)

freeToGo :: Elves -> Direction -> (Int, Int) -> Bool
freeToGo elves dir elf = all ((`Set.notMember` elves) . applyDelta elf . directionDelta) $ lookTo dir
  where
    lookTo North = [North, NorthEast, NorthWest]
    lookTo South = [South, SouthEast, SouthWest]
    lookTo West = [West, NorthWest, SouthWest]
    lookTo East = [East, NorthEast, SouthEast]
    lookTo _ = error "Impossible"

propose :: Elves -> [Direction] -> (Int, Int) -> Maybe (Int, Int)
propose elves pls' elf = do
  proposal <- find (\dir -> freeToGo elves dir elf) pls'
  return $ applyDelta elf $ directionDelta proposal

isBlocked :: Elves -> Elf -> Bool
isBlocked elves elf =
  any
    (\dir -> applyDelta elf (directionDelta dir) `Set.member` elves)
    [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

addProposal :: Map (Int, Int) (Set (Int, Int)) -> (Int, Int) -> Maybe (Int, Int) -> Map (Int, Int) (Set (Int, Int))
addProposal m _ Nothing = m
addProposal m elf (Just p) = Map.insertWith Set.union p (Set.singleton elf) m

round :: Elves -> [Direction] -> Elves
round elves pls' = goodProposalsNewPositions `Set.union` otherElves
  where
    blockedElves = Set.filter (isBlocked elves) elves
    -- proposed position -> set elves
    proposals = foldl (\m elf -> addProposal m elf (propose elves pls' elf)) Map.empty blockedElves
    goodProposals = Map.filter (\s -> Set.size s == 1) proposals
    goodProposalsNewPositions = Map.keysSet goodProposals
    goodProposalElves = Set.unions $ Map.elems goodProposals
    -- Other elves stay put
    otherElves = elves `Set.difference` goodProposalElves

rounds :: Elves -> [Direction] -> [Elves]
rounds elves pls = newElves : rounds newElves newPls
  where
    (_ : newPls) = pls
    newElves = round elves (take 4 pls)

boundingBox :: Elves -> (Int, Int, Int, Int)
boundingBox elves = (Set.findMin xs, Set.findMax xs, Set.findMin ys, Set.findMax ys)
  where
    xs = Set.map fst elves
    ys = Set.map snd elves

boxSize :: (Int, Int, Int, Int) -> Int
boxSize (minX, maxX, minY, maxY) = (maxX - minX + 1) * (maxY - minY + 1)

findAnswer :: Elves -> Int
findAnswer elves = groundTiles
  where
    pls = cycle [North, South, West, East]
    afterTen = rounds elves pls !! 9
    groundTiles = boxSize (boundingBox afterTen) - Set.size afterTen

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let elves = Set.fromList [(x, y) | (y, line) <- zip [0 ..] input, (x, v) <- zip [0 ..] line, v == '#']
  print $ findAnswer elves
