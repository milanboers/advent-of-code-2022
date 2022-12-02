data Move = R | P | S deriving (Eq, Show)

data Strategy = W | D | L deriving (Eq, Show)

shapeScore :: Move -> Int
shapeScore R = 1
shapeScore P = 2
shapeScore S = 3

winScore :: (Move, Move) -> Int
winScore (S, R) = 6
winScore (R, P) = 6
winScore (P, S) = 6
winScore (x, y) | x == y = 3
winScore _ = 0

ownMove :: (Move, Strategy) -> Move
ownMove (R, W) = P
ownMove (P, W) = S
ownMove (S, W) = R
ownMove (R, L) = S
ownMove (P, L) = R
ownMove (S, L) = P
ownMove (x, D) = x

gameScore :: (Move, Strategy) -> Int
gameScore (x, s) = winScore (x, y) + shapeScore y
  where
    y = ownMove (x, s)

findAnswer :: [(Move, Strategy)] -> Int
findAnswer = sum . map gameScore

parseOpponentMove :: Char -> Move
parseOpponentMove 'A' = R
parseOpponentMove 'B' = P
parseOpponentMove 'C' = S
parseOpponentMove _ = error "Invalid opponenent move"

parseStrategy :: Char -> Strategy
parseStrategy 'X' = L
parseStrategy 'Y' = D
parseStrategy 'Z' = W
parseStrategy _ = error "Invalid own move"

parseGame :: String -> (Move, Strategy)
parseGame (x : _ : y : "") = (parseOpponentMove x, parseStrategy y)
parseGame _ = error "Invalid game line"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let games = map parseGame input
  print $ findAnswer games
