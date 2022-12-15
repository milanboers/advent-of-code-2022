import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Ord

mhd :: (Int, Int) -> (Int, Int) -> Int
mhd (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distancesToBeacon :: [((Int, Int), (Int, Int))] -> [((Int, Int), Int)]
distancesToBeacon = sortOn (Data.Ord.Down . snd) . map (\(s, b) -> (s, mhd s b))

perimiter :: ((Int, Int), Int) -> [(Int, Int)]
perimiter ((x, y), d) = l1 ++ l2 ++ l3 ++ l4
  where
    t = (x, y - d - 1)
    r = (x + d + 1, y)
    b = (x, y + d + 1)
    l = (x - d - 1, y)
    l1 = takeWhile (/= r) . iterate (\(x', y') -> (x' + 1, y' + 1)) $ t
    l2 = takeWhile (/= b) . iterate (\(x', y') -> (x' - 1, y' + 1)) $ r
    l3 = takeWhile (/= l) . iterate (\(x', y') -> (x' - 1, y' - 1)) $ b
    l4 = takeWhile (/= t) . iterate (\(x', y') -> (x' + 1, y' - 1)) $ l

blockedBySensor :: ((Int, Int), Int) -> (Int, Int) -> Bool
blockedBySensor ((sx, sy), sd) (x, y) = mhd (sx, sy) (x, y) <= sd

blockedForBeacon :: [((Int, Int), Int)] -> (Int, Int) -> Bool
blockedForBeacon sensors (x, y) = any (\s -> blockedBySensor s (x, y)) sensors

findAnswer :: [((Int, Int), (Int, Int))] -> Int
findAnswer sbs = beaconX * 4000000 + beaconY
  where
    sensors = distancesToBeacon sbs

    beaconCannotExist = blockedForBeacon sensors
    allPerimiters = concatMap perimiter sensors
    (beaconX, beaconY) =
      fromJust $
        find (not . beaconCannotExist) $
          [ (x, y) | (x, y) <- allPerimiters, x >= 0, x <= 4000000, y >= 0, y <= 4000000
          ]

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
