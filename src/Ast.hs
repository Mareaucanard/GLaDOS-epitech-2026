{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast
--}
module Ast (sexprToAST, evalAst, VarMap, mapEvalCalls, createLambda) where

import qualified Data.Map.Lazy as Map
import           Lib (myMaybeMap)
import           Types (Ast(..), SExpr(..), VarMap)
import           System.Random (StdGen, mkStdGen, uniform)

-- |Checks list of SExpr.
checkOperands :: [SExpr] -- ^ The list of SExpr to check
              -> Either [Ast] String -- ^ The return value
checkOperands = myMaybeMap sexprToAST

-- |Extracts symbols from a list of SExpr.
extractSymbols :: [SExpr] -- ^ The list of SExpr to extract
  -> Either [String] String -- ^ The return value
extractSymbols (Symbol x : xs) = case extractSymbols xs of
  Right err -> Right err
  Left list -> Left (x:list)
extractSymbols [] = Left []
extractSymbols _ = Right "Can't extract (not a symbol)"

-- |Extracts sym from a list of Asts.
extractSym :: [Ast] -- ^ The list of Ast to extract
  -> Either [String] String -- ^ The return value
extractSym (Sym x : xs) = case extractSym xs of
  Right err -> Right err
  Left list -> Left (x:list)
extractSym [] = Left []
extractSym _ = Right "Can't extract (not a symbol)"

-- |Handles lambdas.
handleLambda :: [SExpr] -- ^ The list of SExpr to handle
  -> Either Ast String -- ^ The return value
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
sexprToAST (List (Symbol "lambda":xs)) = handleLambda xs
sexprToAST (List (s:xs)) = case sexprToAST s of
  Right msg -> Right msg
  Left es   -> case checkOperands xs of
    Right err -> Right err
    Left exs  -> Left (Call es exs)
sexprToAST (List []) = Left None

-- |Tries to apply a call or lambda.
applyOp :: Ast -- ^ The operator
        -> [Ast] -- ^ The arguments
        -> VarMap -- ^ The map of variables
        -> Either (Ast, VarMap) String -- ^ The return value
applyOp (Value _) _ _ = Right "Can't apply on number"
applyOp (Tab _) _ _ = Right "Can't apply on list"
applyOp (Call o l) args m = Left (Call (Call o l) args, m)
applyOp None _ _ = Right "Can't apply on an empty function"
applyOp (Lambda l) args m = case l args m of
  Right msg -> Right msg
  Left v    -> evalAst v m
applyOp (Boolean _) _ _ = Right "Can't apply on boolean"
applyOp _ _ _ = Right "Can't apply"  -- Should never be reached

-- |Tries to add to a Either list.
-- If there's an error, returns the first error found.
addMapEvalCalls :: Ast -- ^ The Ast to add
                -> Either ([Ast], VarMap) String -- ^ The either list to add to
                -> Either ([Ast], VarMap) String -- ^ The return value
addMapEvalCalls _ (Right msg) = Right msg
addMapEvalCalls x (Left (xs, m)) = Left (x:xs, m)

-- |For evaluating a list of arguments.
mapEvalCalls :: [Ast] -- ^ The list of arguments
             -> VarMap -- ^ The map of variables
             -> Either ([Ast], VarMap) String -- ^ The return value
mapEvalCalls [] m = Left ([], m)
mapEvalCalls (x:xs) m = case evalAst x m of
  Right msg     -> Right msg
  Left (ex, em) -> addMapEvalCalls ex (mapEvalCalls xs em)

--rand :: [Ast] -> VarMap -> Either Ast String
-- rand [] m = case Map.lookup "seed" m of
--   Just (Value seed) = Left
--   Nothing -> Right $ "Invalid seed, please set it to a value"
-- |Tries to evaluate a call.
-- Special keywords are forked here.
evalCall :: Ast -- ^ The operator
         -> [Ast] -- ^ The arguments
         -> VarMap -- ^ The map of variables
         -> Either (Ast, VarMap) String -- ^ The return value
evalCall (Sym "rand") [] m = case Map.lookup "seed" m of
  Just (Value seed) -> Left (Value rand, Map.insert "seed" (Value rand) m)
    where
      rand = fst (uniform (mkStdGen seed) :: (Int, StdGen))
  _ -> Right "Invalid seed, please set it to a value"
evalCall (Sym "rand") _ _ = Right "Rand only takes one argument"
evalCall (Sym "define") arg m = case defineSymbol arg m of
  Right msg   -> Right msg
  Left (s, v) -> Left (None, Map.insert s v m)
evalCall o l m = case evalAst o m of
  Right msg     -> Right msg
  Left (eo, em) -> applyOp eo l em

-- |Tries to read a variable.
-- On error returns a Right string with a description of the error.
-- On success returns on the left a tuple with the evaluated expression and new map.
tryReadVar :: String -- ^ The key to read
           -> VarMap -- ^ The map of variables
           -> Either (Ast, VarMap) String -- ^ The return value
tryReadVar key m = case Map.lookup key m of
  Nothing -> Right $ "Unknown symbol " ++ key
  Just v  -> Left (v, m)

handleNamedFunction :: [Ast] -- ^ The name of the arguments of the function
    -> Ast -- ^ The function to be called
    -> Either Ast String  -- ^ On success new var map else error message
handleNamedFunction args eval = case extractSym args of
  Right _ -> Right "Named function arguments must be symbols"
  Left argNames -> Left $ createLambda argNames eval

-- |Handles the special keyword define.
-- On error returns a Right string with a description of the error.
-- On success returns on the left a tuple with the evaluated expression and new map.
defineSymbol :: [Ast] -- ^ The arguments
             -> VarMap -- ^ The map of variables
             -> Either (String, Ast) String -- ^ The return value
defineSymbol [Sym s, v] m = case evalAst v m of
  Right msg      -> Right msg
  Left (eo, _)   -> Left (s, eo)
defineSymbol [Call (Sym name) args, v] _ = case handleNamedFunction args v of
  Right msg -> Right msg
  Left newLambda -> Left (name, newLambda)
defineSymbol [_, _] _ = Right "Can only define a symbol or call"
defineSymbol _ _ = Right "Define takes exactly two arguments"

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

-- |Evaluates a lambda.
-- On error returns a Right string with a description of the error.
-- On success returns on the left the evaluated expression.
evalLambda :: [String] -- ^ The list of arguments names
  -> Ast -- ^ The expression to evaluate
  -> [Ast] -- ^ The arguments
  -> VarMap -- ^ The map of variables
  -> Either Ast String -- ^ The return value
evalLambda argsName expr args vars = case insertLambdaVars argsName args vars of
  Right err1   -> Right err1
  Left newVars -> case evalAst expr newVars of
    Right err2        -> Right err2
    Left (newExpr, _) -> case evalAst newExpr newVars of
      Right message    -> Right message
      Left (retVal, _) -> Left retVal

-- |Inserts a new element in a map
insertLambda :: String -- ^ The key to insert
  -> Ast -- ^ The value to insert
  -> Either VarMap String -- ^ The either map to insert to
  -> Either VarMap String -- ^ The return value
insertLambda _ _ (Right err) = Right err
insertLambda key val (Left m) = Left $ Map.insert key val m

-- |Inserts a list of variables in a map
insertLambdaVars :: [String] -- ^ The list of keys to insert
  -> [Ast] -- ^ The list of values to insert
  -> VarMap -- ^ The map to insert to
  -> Either VarMap String -- ^ The return value
insertLambdaVars (name : names) (var : vars) vmap = case evalAst var vmap of
  Right msg -> Right msg
  Left (evaledVar, _) -> insertLambda name evaledVar nmap
  where
    nmap = insertLambdaVars names vars vmap
insertLambdaVars [] [] m = Left m
insertLambdaVars _ [] _ = Right "Not enough arguments"
insertLambdaVars [] _ _ = Right "Too many arguments"

-- |Creates a lambda.
createLambda :: [String] -- ^ The list of arguments names
  -> Ast -- ^ The expression to evaluate
  -> Ast -- ^ The return value
createLambda args ast = Lambda (evalLambda args ast)
