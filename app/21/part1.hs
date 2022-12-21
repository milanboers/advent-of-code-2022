{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data Op = Add | Sub | Mul | Div

data Monkey = Number String Int | Operation String Op String String

type Monkeys = Map String Monkey

name :: Monkey -> String
name (Number n _) = n
name (Operation n _ _ _) = n

value :: Monkeys -> String -> Int
value mm mn = case monkey of
  (Number _ x) -> x
  (Operation _ Add m1 m2) -> value mm m1 + value mm m2
  (Operation _ Sub m1 m2) -> value mm m1 - value mm m2
  (Operation _ Mul m1 m2) -> value mm m1 * value mm m2
  (Operation _ Div m1 m2) -> value mm m1 `div` value mm m2
  where
    monkey = mm Map.! mn

findAnswer :: Monkeys -> Int
findAnswer mm = value mm "root"

parseOp :: String -> (Op, String, String)
parseOp s = case op of
  "+" -> (Add, m1, m2)
  "-" -> (Sub, m1, m2)
  "*" -> (Mul, m1, m2)
  "/" -> (Div, m1, m2)
  _ -> error "Invalid operation"
  where
    [m1, op, m2] = splitOn " " s

parseLine :: String -> Monkey
parseLine line = case readMaybe rest of
  Just x -> Number name' x
  Nothing -> Operation name' op m1 m2
    where
      (op, m1, m2) = parseOp rest
  where
    [name', rest] = splitOn ": " line

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let monkeys = map parseLine input
  let mm = Map.fromList $ map (\m -> (name m, m)) monkeys
  print $ findAnswer mm
