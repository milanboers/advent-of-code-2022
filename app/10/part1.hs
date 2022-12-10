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

findAnswer :: [(Int, Int)] -> Int
findAnswer cmds = sum . map (\t -> t * valueDuringCycle vac t) $ [20, 60, 100, 140, 180, 220]
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
  print $ findAnswer commands
