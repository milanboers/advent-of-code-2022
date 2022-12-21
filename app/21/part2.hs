{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data Op = Add | Sub | Mul | Div deriving (Show, Eq)

data Monkey = Number String Int | Operation String Op String String deriving (Show, Eq)

type Monkeys = Map String Monkey

data Expression = VarExp | NumExp Int | OpExp Op Expression Expression deriving (Show, Eq)

name :: Monkey -> String
name (Number n _) = n
name (Operation n _ _ _) = n

applyOp :: Op -> Int -> Int -> Int
applyOp Add x y = x + y
applyOp Sub x y = x - y
applyOp Mul x y = x * y
applyOp Div x y = x `div` y

reduce :: Expression -> Expression
reduce (OpExp op nx ny) = case (rx, ry) of
  (NumExp x, NumExp y) -> NumExp (applyOp op x y)
  _ -> OpExp op rx ry
  where
    rx = reduce nx
    ry = reduce ny
reduce e = e

findVarVal :: Expression -> Int -> Int
findVarVal VarExp n = n
findVarVal (OpExp Add (NumExp x) ny) n = findVarVal ny (n - x)
findVarVal (OpExp Add ny (NumExp x)) n = findVarVal ny (n - x)
findVarVal (OpExp Sub (NumExp x) ny) n = findVarVal ny (-n + x)
findVarVal (OpExp Sub ny (NumExp x)) n = findVarVal ny (n + x)
findVarVal (OpExp Mul (NumExp x) ny) n = findVarVal ny (n `div` x)
findVarVal (OpExp Mul ny (NumExp x)) n = findVarVal ny (n `div` x)
findVarVal (OpExp Div (NumExp x) ny) n = findVarVal ny (x `div` n)
findVarVal (OpExp Div ny (NumExp x)) n = findVarVal ny (n * x)
findVarVal (NumExp _) _ = error "No variable in expression"
findVarVal _ _ = error "Cannot find var value"

findVarValEqs :: Expression -> Expression -> Int
findVarValEqs ex1 ex2 = case (reduce ex1, reduce ex2) of
  (NumExp x, ex) -> findVarVal ex x
  (ex, NumExp x) -> findVarVal ex x
  _ -> error "Variables on both sides."

toExp :: Monkeys -> Monkey -> Expression
toExp _ (Number "humn" _) = VarExp
toExp _ (Number _ x) = NumExp x
toExp mm (Operation _ op m1 m2) = OpExp op (toExp' m1) (toExp' m2)
  where
    toExp' m = toExp mm (mm Map.! m)

findAnswer :: Monkeys -> Int
findAnswer mm = findVarValEqs exp1 exp2
  where
    (Operation _ _ m1 m2) = mm Map.! "root"
    exp1 = toExp mm (mm Map.! m1)
    exp2 = toExp mm (mm Map.! m2)

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
