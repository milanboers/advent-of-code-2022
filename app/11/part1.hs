import Data.List (isInfixOf, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Read (readMaybe)

data Operator = Add | Mul deriving (Eq, Show)

data Monkey = Monkey
  { startingItems :: [Int],
    operation :: (Operator, Maybe Int),
    test :: (Int, Int, Int)
  }
  deriving (Eq, Show)

type Monkeys = Map Int Monkey

data MonkeyState = MonkeyState
  { items :: [Int],
    inspections :: Int
  }
  deriving (Eq, Show)

type State = Map Int MonkeyState

applyOperation :: (Operator, Maybe Int) -> Int -> Int
applyOperation (Add, Just y) x = x + y
applyOperation (Mul, Just y) x = x * y
applyOperation (op, Nothing) x = applyOperation (op, Just x) x

-- inspect first item for monkey
inspect :: Monkeys -> State -> Int -> State
inspect m s mi = case s ! mi of
  MonkeyState [] _ -> undefined
  MonkeyState (worryLevel : newMonkeyItems) ins -> newState
    where
      Monkey _ operation' (testX, trueMonkey, falseMonkey) = m ! mi

      newMonkeyState = MonkeyState newMonkeyItems (ins + 1)

      newWorryLevel = applyOperation operation' worryLevel `div` 3
      toMonkeyI = if newWorryLevel `mod` testX == 0 then trueMonkey else falseMonkey
      MonkeyState toMonkeyItems toMonkeyInspections = s ! toMonkeyI

      newToMonkeyState = MonkeyState (toMonkeyItems ++ [newWorryLevel]) toMonkeyInspections

      newState = Map.union (Map.fromList [(mi, newMonkeyState), (toMonkeyI, newToMonkeyState)]) s

takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne p xs = case span p xs of
  (prefix, []) -> prefix
  (prefix, x : _) -> prefix ++ [x]

turn :: Monkeys -> State -> Int -> State
turn m s mi = last . takeWhilePlusOne (\s' -> not (null (items (s' ! mi)))) . iterate (\s' -> inspect m s' mi) $ s

round' :: Monkeys -> State -> State
round' m s = foldl (turn m) s (Map.keys s)

findAnswer :: Monkeys -> Int
findAnswer monkeys = product . take 2 . reverse . sort . map (inspections . snd) . Map.toList $ afterRounds
  where
    initialState = Map.map (\m -> MonkeyState (startingItems m) 0) monkeys
    afterRounds = (!! 20) . iterate (round' monkeys) $ initialState

parseMonkey :: Monkeys -> [String] -> Monkeys
parseMonkey m [l1, l2, l3, l4, l5, l6] = Map.insert i monkey m
  where
    readIntFromEnd = read . last . splitOn " "

    i = readIntFromEnd . init $ l1
    startingItems' = map read . splitOn "," . filter (/= ' ') . (!! 1) . splitOn ":" $ l2
    operator = if "*" `isInfixOf` l3 then Mul else Add
    operationI = readMaybe . last . splitOn " " $ l3
    testDivBy = readIntFromEnd l4
    testTrue = readIntFromEnd l5
    testFalse = readIntFromEnd l6

    monkey = Monkey startingItems' (operator, operationI) (testDivBy, testTrue, testFalse)
parseMonkey _ _ = error ""

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let monkeysS = splitOn [""] input
  let monkeys = foldl parseMonkey Map.empty monkeysS
  print $ findAnswer monkeys
