import Data.Char (isDigit)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import qualified Data.Ord
import qualified Data.Set as Set

type Dimensions = (Int, Int, Int, Int)

mhd :: (Int, Int) -> (Int, Int) -> Int
mhd (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distancesToBeacon :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
distancesToBeacon = sortOn (Data.Ord.Down . snd) . map (\(s, b) -> (s, mhd s b))

dimensions :: [((Int, Int), Int)] -> Dimensions
dimensions sensors = (minimum xs - maxD, maximum xs + maxD, minimum ys - maxD, maximum ys + maxD)
  where
    xs = map (\((x, _), _) -> x) sensors
    ys = map (\((_, y), _) -> y) sensors
    maxD = maximum . map snd $ sensors

rowCoords :: Dimensions -> Int -> [(Int, Int)]
rowCoords (minX, maxX, _, _) rowY = [(x, rowY) | x <- [minX .. maxX]]

blockedBySensor :: ((Int, Int), Int) -> (Int, Int) -> Bool
blockedBySensor ((sx, sy), sd) (x, y) = mhd (sx, sy) (x, y) <= sd

blockedForBeacon :: [((Int, Int), Int)] -> (Int, Int) -> Bool
blockedForBeacon sensors (x, y) = any (\s -> blockedBySensor s (x, y)) sensors

findAnswer :: [((Int, Int), (Int, Int))] -> Int
findAnswer sbs = length . filter beaconCannotExist $ rowCoords dims 2000000
  where
    sensors = distancesToBeacon sbs
    dims = dimensions sensors
    beaconSet = Set.fromList $ map snd sbs

    beaconCannotExist pos = blockedForBeacon sensors pos && pos `Set.notMember` beaconSet

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = toPair . map (read . takeWhile isNumeric) . tail . splitOn "="
  where
    toPair [sx, sy, bx, by] = ((sx, sy), (bx, by))
    toPair _ = error "Invalid line"

    isNumeric x = isDigit x || x == '-'

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let pairs = map parseLine input
  print $ findAnswer pairs
