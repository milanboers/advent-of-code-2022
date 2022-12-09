import Data.List (nub)
import Data.List.Split (splitOn)

limitD :: Int -> Int
limitD x | x < -1 = -1
limitD x | x > 1 = 1
limitD x = x

tailPosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailPosition (tx, ty) (hx, hy) = if abs dx > 1 || abs dy > 1 then (tx + moveX, ty + moveY) else (tx, ty)
  where
    (dx, dy) = (tx - hx, ty - hy)
    (moveX, moveY) = (limitD (-dx), limitD (-dy))

tailPositions :: [(Int, Int)] -> [(Int, Int)]
tailPositions = scanl tailPosition (0, 0)

headPosition :: (Int, Int) -> Char -> (Int, Int)
headPosition (x, y) 'R' = (x + 1, y)
headPosition (x, y) 'L' = (x - 1, y)
headPosition (x, y) 'U' = (x, y - 1)
headPosition (x, y) 'D' = (x, y + 1)
headPosition _ _ = error "Invalid command"

headPositions :: [Char] -> [(Int, Int)]
headPositions = scanl headPosition (0, 0)

findAnswer :: [(Char, Int)] -> Int
findAnswer cmds = length . nub . (!! 9) . iterate tailPositions . headPositions $ expandedCmds
  where
    expandedCmds = concatMap (\(c, n) -> replicate n c) cmds

parseLine :: String -> (Char, Int)
parseLine line = case splitOn " " line of
  [[cmd], n] -> (cmd, read n)
  _ -> error "Invalid line"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let commands = map parseLine input
  print $ findAnswer commands
