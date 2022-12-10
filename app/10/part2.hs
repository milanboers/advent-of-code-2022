import Data.List.Split (chunksOf)

valuesAtCycles :: [(Int, Int)] -> [(Int, Int)]
valuesAtCycles cmds = (0, 1) : zip timestamps values
  where
    values = map (+ 1) . scanl1 (+) . map snd $ cmds
    timestamps = scanl1 (+) . map fst $ cmds

-- values -> time -> value
valueDuringCycle :: [(Int, Int)] -> Int -> Int
valueDuringCycle ((xt, xv) : (yt, _) : _) t | xt < t && t <= yt = xv
valueDuringCycle (_ : xs) t = valueDuringCycle xs t
valueDuringCycle [] _ = error "Invalid time"

-- sprite position -> cycle -> pixel
pixelValue :: Int -> Int -> Char
pixelValue p c = if isLit then '#' else '.'
  where
    isLit = abs (p - ((c - 1) `mod` 40)) <= 1

findAnswer :: [(Int, Int)] -> [String]
findAnswer cmds = chunksOf 40 . map (\t -> pixelValue (valueDuringCycle vac t) t) $ [1 .. 240]
  where
    vac = valuesAtCycles cmds

-- line -> (duration, x)
parseLine :: String -> (Int, Int)
parseLine "noop" = (1, 0)
parseLine ('a' : 'd' : 'd' : 'x' : ' ' : x) = (2, read x)
parseLine _ = error "Invalid line"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let commands = map parseLine input
  putStr $ unlines $ findAnswer commands
