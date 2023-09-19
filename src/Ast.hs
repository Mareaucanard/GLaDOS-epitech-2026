{-# LANGUAGE InstanceSigs #-}

module Ast (Ast (..), sexprToAST, evalAst, VarMap, defaultSymbols) where

import qualified Data.Map.Lazy as Map
import Lib (SExpr (..), myMaybeMap)

type VarMap = Map.Map String Ast

data Ast
  = Value Int
  | Sym String
  | Call Ast [Ast]
  | Boolean Bool
  | Lambda ([Ast] -> Either Ast String)
  | None

instance Show Ast where
  show :: Ast -> String
  show (Lambda _) = "Lambda"
  show (Value i) = "Value " ++ show i
  show (Sym s) = "Symbol " ++ show s
  show (Call a b) = "Call " ++ show a ++ " " ++ show b
  show (Boolean b) = "Boolean " ++ show b
  show None = "None"

checkOperands :: [SExpr] -> Either [Ast] String
checkOperands = myMaybeMap sexprToAST

sexprToAST :: SExpr -> Either Ast String
sexprToAST (Integer i) = Left (Value i)
sexprToAST (Symbol s) = Left (Sym s)
sexprToAST (List [sexpr]) = sexprToAST sexpr
sexprToAST (List (s : xs)) = case sexprToAST s of
  Right msg -> Right msg
  Left es -> case checkOperands xs of
    Right err -> Right err
    Left exs -> Left (Call es exs)
sexprToAST (List []) = Left None

applyOp :: Ast -> [Ast] -> VarMap -> Either (Ast, VarMap) String
applyOp (Value v) _ _ = Right $ "Can't apply on value " ++ show v
applyOp (Sym s) _ _ = Right $ "Wrong use of symbol " ++ s
applyOp (Call o l) args m = Left (Call (Call o l) args, m)
applyOp None _ _ = Right "Can't apply on an empty function"
applyOp (Lambda l) args m = case l args of
  Right msg -> Right msg
  Left v -> evalAst v m
applyOp (Boolean _) _ _ = Right "Can't apply on boolean"

addMapEvalCalls :: Ast -> Either ([Ast], VarMap) String -> Either ([Ast], VarMap) String
addMapEvalCalls _ (Right msg) = Right msg
addMapEvalCalls x (Left (xs, m)) = Left (x : xs, m)

mapEvalCalls :: [Ast] -> VarMap -> Either ([Ast], VarMap) String
mapEvalCalls [] m = Left ([], m)
mapEvalCalls (x : xs) m = case evalAst x m of
  Right msg -> Right msg
  Left (ex, em) -> addMapEvalCalls ex (mapEvalCalls xs em)

evalCall :: Ast -> [Ast] -> VarMap -> Either (Ast, VarMap) String
evalCall (Sym "define") arg m = case defineSymbol arg m of
  Right msg -> Right msg
  Left (s, v) -> Left (None, Map.insert s v m)
evalCall o l m = case evalAst o m of
  Right msg -> Right msg
  Left (eo, em) -> case mapEvalCalls l em of
    Right err -> Right err
    Left (el, em2) -> applyOp eo el em2

tryReadVar :: String -> VarMap -> Either (Ast, VarMap) String
tryReadVar key m = case Map.lookup key m of
  Nothing -> Right $ "Unknown symbol " ++ key
  Just v -> Left (v, m)

defineSymbol :: [Ast] -> VarMap -> Either (String, Ast) String
defineSymbol [Sym s, v] m = case evalAst v m of
  Right msg -> Right msg
  Left (None, _) -> Right "Can't define symbol to None"
  Left (eo, _) -> Left (s, eo)
defineSymbol [s, _] _ = Right $ "Can only define a symbol, not " ++ show s
defineSymbol _ _ = Right "define takes exactly two argument"

addAst :: [Ast] -> Either Ast String
addAst [Value a, Value b] = Left (Value (a + b))
addAst _ = Right "Invalid use of add operation"

subAst :: [Ast] -> Either Ast String
subAst [Value a, Value b] = Left (Value (a - b))
subAst _ = Right "Invalid use of sub operation"

mulAst :: [Ast] -> Either Ast String
mulAst [Value a, Value b] = Left (Value (a * b))
mulAst _ = Right "Invalid use of times operation"

divAst :: [Ast] -> Either Ast String
divAst [Value _, Value 0] = Right "Division by zero"
divAst [Value a, Value b] = Left (Value (a `quot` b))
divAst _ = Right "Invalid use of div operation"

modAst :: [Ast] -> Either Ast String
modAst [Value _, Value 0] = Right "Module by zero"
modAst [Value a, Value b] = Left (Value (a `mod` b))
modAst _ = Right "Invalid use of mod operation"

powAst :: [Ast] -> Either Ast String
powAst [Value a, Value b] = Left (Value (a ^ b))
powAst _ = Right "Invalid use of power operation"

defaultSymbols :: VarMap
defaultSymbols =
  Map.fromList
    [ ("+", Lambda addAst),
      ("-", Lambda subAst),
      ("*", Lambda mulAst),
      ("/", Lambda divAst),
      ("%", Lambda modAst),
      ("^", Lambda powAst)
    ]

evalAst :: Ast -> VarMap -> Either (Ast, VarMap) String
evalAst (Value i) m = Left (Value i, m)
evalAst (Sym s) m = tryReadVar s m
evalAst (Call o l) m = evalCall o l m
evalAst (Lambda x) m = Left (Lambda x, m)
evalAst None m = Left (None, m)
evalAst (Boolean b) m = Left (Boolean b, m)
