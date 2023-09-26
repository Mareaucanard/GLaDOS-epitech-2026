{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast
--}

module Ast (sexprToAST, evalAst, VarMap, mapEvalCalls) where

import qualified Data.Map.Lazy as Map
import Lib (myMaybeMap)
import Types (Ast (..), SExpr (..), VarMap)

-- |Checks list of SExpr.
checkOperands :: [SExpr] -- ^ The list of SExpr to check
  -> Either [Ast] String -- ^ The return value
checkOperands = myMaybeMap sexprToAST

extractSymbols :: [SExpr] -> Either [String] String
extractSymbols (Symbol x : xs) = case extractSymbols xs of
  Right err -> Right err
  Left list -> Left (x : list)
extractSymbols [] = Left []
extractSymbols _ = Right "Can't extract (not a symbol)"

handleLambda :: [SExpr] -> Either Ast String
handleLambda [List argNames, expr] =
  case extractSymbols argNames of
    Right _ -> Right "Lambda first argument must be only symbols"
    Left strings -> case sexprToAST expr of
      Right err -> Right err
      Left ast -> Left $ createLambda strings ast
handleLambda _ = Right "Invalid number of arguments for lambda"

-- |Transforms a SExpr into an Ast.
-- On error, returns a string on the right explaining an error.
-- On success, returns the Ast.
sexprToAST :: SExpr -- ^ The SExpr to transform
  -> Either Ast String -- ^ The return value
sexprToAST (Integer i) = Left (Value i)
sexprToAST (Boolan b) = Left (Boolean b)
sexprToAST (Symbol s) = Left (Sym s)
sexprToAST (List (Symbol "lambda" : xs)) = handleLambda xs
sexprToAST (List [sexpr]) = sexprToAST sexpr
sexprToAST (List (s : xs)) = case sexprToAST s of
  Right msg -> Right msg
  Left es -> case checkOperands xs of
    Right err -> Right err
    Left exs -> Left (Call es exs)
sexprToAST (List []) = Left None

-- |Tries to apply a call or lambda.
applyOp :: Ast -- ^ The operator
  -> [Ast] -- ^ The arguments
  -> VarMap -- ^ The map of variables
  -> Either (Ast, VarMap) String -- ^ The return value
applyOp (Value v) _ _ = Right $ "Can't apply on value " ++ show v
applyOp (Tab t) _ _ = Right $ "Can't apply on tab " ++ show t
applyOp (Sym s) _ _ = Right $ "Wrong use of symbol " ++ s
applyOp (Call o l) args m = Left (Call (Call o l) args, m)
applyOp None _ _ = Right "Can't apply on an empty function"
applyOp (Lambda l) args m = case l args m of
  Right msg -> Right msg
  Left v -> evalAst v m
applyOp (Boolean _) _ _ = Right "Can't apply on boolean"

-- |Tries to add to a Either list.
-- If there's an error, returns the first error found.
addMapEvalCalls :: Ast -- ^ The Ast to add
  -> Either ([Ast], VarMap) String -- ^ The either list to add to
  -> Either ([Ast], VarMap) String -- ^ The return value
addMapEvalCalls _ (Right msg) = Right msg
addMapEvalCalls x (Left (xs, m)) = Left (x : xs, m)

-- |For evaluating a list of arguments.
mapEvalCalls :: [Ast] -- ^ The list of arguments
  -> VarMap -- ^ The map of variables
  -> Either ([Ast], VarMap) String -- ^ The return value
mapEvalCalls [] m = Left ([], m)
mapEvalCalls (x : xs) m = case evalAst x m of
  Right msg -> Right msg
  Left (ex, em) -> addMapEvalCalls ex (mapEvalCalls xs em)

-- |Tries to evaluate a call.
-- Special keywords are forked here.
evalCall :: Ast -- ^ The operator
  -> [Ast] -- ^ The arguments
  -> VarMap -- ^ The map of variables
  -> Either (Ast, VarMap) String -- ^ The return value
evalCall (Sym "define") arg m = case defineSymbol arg m of
  Right msg -> Right msg
  Left (s, v) -> Left (None, Map.insert s v m)
evalCall o l m = case evalAst o m of
  Right msg -> Right msg
  Left (eo, em) -> applyOp eo l em


-- |Tries to read a variable.
tryReadVar :: String -- ^ The key to read
  -> VarMap -- ^ The map of variables
  -> Either (Ast, VarMap) String -- ^ The return value
tryReadVar key m = case Map.lookup key m of
  Nothing -> Right $ "Unknown symbol " ++ key
  Just v -> Left (v, m)

-- |Handles the special keyword define.
defineSymbol :: [Ast] -- ^ The arguments
  -> VarMap -- ^ The map of variables
  -> Either (String, Ast) String -- ^ The return value
defineSymbol [Sym s, v] m = case evalAst v m of
  Right msg -> Right msg
  Left (None, _) -> Right "Can't define symbol to None"
  Left (eo, _) -> Left (s, eo)
defineSymbol [s, _] _ = Right $ "Can only define a symbol, not " ++ show s
defineSymbol _ _ = Right "define takes exactly two argument"

-- |Evaluates an Ast and also update variables if there's a define.
-- On error returns a Right string with a description of the error.
-- On success returns on the left a tuple with the evaluated expression and new map.
evalAst :: Ast -- ^ The Ast to evaluate
  -> VarMap -- ^ The map of variables
  -> Either (Ast, VarMap) String -- ^ The return value
evalAst (Value i) m = Left (Value i, m)
evalAst (Sym s) m = tryReadVar s m
evalAst (Call o l) m = evalCall o l m
evalAst (Lambda x) m = Left (Lambda x, m)
evalAst None m = Left (None, m)
evalAst (Boolean b) m = Left (Boolean b, m)
evalAst (Tab t) m = Left (Tab t, m)

{--
    evalLambda [ArgName1, ArgName2, ...] Expression ArgList
--}
evalLambda :: [String] -> Ast -> [Ast] -> VarMap -> Either Ast String
evalLambda argsName expr args vars = case insertLambdaVars argsName args vars of
  Right err1 -> Right err1
  Left newVars -> case evalAst expr newVars of
      Right err2 -> Right err2
      Left (newExpr, _) -> case evalAst newExpr newVars of
        Right message -> Right message
        Left (retVal, _) -> Left retVal

{--
    Inserts a new element in a map
--}
insertLambda :: String -> Ast -> Either VarMap String -> Either VarMap String
insertLambda _ _ (Right err) = Right err
insertLambda key val (Left m) = Left $ Map.insert key val m

{--
    Inserts multiple new elements in a map
--}
insertLambdaVars :: [String] -> [Ast] -> VarMap -> Either VarMap String
insertLambdaVars (name : names) (var : vars) vmap = case evalAst var vmap of
  Right msg -> Right msg
  Left (evaledVar, _) -> insertLambda name evaledVar nmap
  where
    nmap = insertLambdaVars names vars vmap
insertLambdaVars [] [] m = Left m
insertLambdaVars [] _ _ = Right "Not enough arguments"
insertLambdaVars _ [] _ = Right "Too many arguments"

{--
    createLambda Name [ArgName1, ArgName2, ..., ArgNameN] Expression
--}
createLambda :: [String] -> Ast -> Ast
createLambda args ast = Lambda (evalLambda args ast)
