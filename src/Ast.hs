module Ast (sexprToAST, evalAst, VarMap) where

import qualified Data.Map.Lazy as Map
import Lib (myMaybeMap)
import Types (Ast (..), SExpr (..), VarMap)

{--
  Check list of SExpr
--}
checkOperands :: [SExpr] -> Either [Ast] String
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

{--
  Transforms a SExpr into an Ast
  On error, returns a string on the right explaining an error
  On success, returns the Ast
--}
sexprToAST :: SExpr -> Either Ast String
sexprToAST (Integer i) = Left (Value i)
sexprToAST (Symbol s) = Left (Sym s)
sexprToAST (Boolan b) = Left (Boolean b)
sexprToAST (List (Symbol "lambda" : xs)) = handleLambda xs
sexprToAST (List [sexpr]) = sexprToAST sexpr
sexprToAST (List (s : xs)) = case sexprToAST s of
  Right msg -> Right msg
  Left es -> case checkOperands xs of
    Right err -> Right err
    Left exs -> Left (Call es exs)
sexprToAST (List []) = Left None

{--
  Tries to apply a call or lambda
--}
applyOp :: Ast -> [Ast] -> VarMap -> Either (Ast, VarMap) String
applyOp (Value v) _ _ = Right $ "Can't apply on value " ++ show v
applyOp (Sym s) _ _ = Right $ "Wrong use of symbol " ++ s
applyOp (Call o l) args m = Left (Call (Call o l) args, m)
applyOp None _ _ = Right "Can't apply on an empty function"
applyOp (Lambda l) args m = case l args m of
  Right msg -> Right msg
  Left v -> evalAst v m
applyOp (Boolean _) _ _ = Right "Can't apply on boolean"

{--
  Tries to add to a Either list
  If there's an error, returns the first error found
--}
addMapEvalCalls :: Ast -> Either ([Ast], VarMap) String -> Either ([Ast], VarMap) String
addMapEvalCalls _ (Right msg) = Right msg
addMapEvalCalls x (Left (xs, m)) = Left (x : xs, m)

{--
  For evaluating a list of arguments
--}
mapEvalCalls :: [Ast] -> VarMap -> Either ([Ast], VarMap) String
mapEvalCalls [] m = Left ([], m)
mapEvalCalls (x : xs) m = case evalAst x m of
  Right msg -> Right msg
  Left (ex, em) -> addMapEvalCalls ex (mapEvalCalls xs em)

{--
  Tries to evaluate a call
  Special keywords are forked here
--}
evalCall :: Ast -> [Ast] -> VarMap -> Either (Ast, VarMap) String
evalCall (Sym "define") arg m = case defineSymbol arg m of
  Right msg -> Right msg
  Left (s, v) -> Left (None, Map.insert s v m)
evalCall o l m = case evalAst o m of
  Right msg -> Right msg
  Left (eo, em) -> case mapEvalCalls l em of
    Right err -> Right err
    Left (el, em2) -> applyOp eo el em2

{--
  Tries to read a variable
--}
tryReadVar :: String -> VarMap -> Either (Ast, VarMap) String
tryReadVar key m = case Map.lookup key m of
  Nothing -> Right $ "Unknown symbol " ++ key
  Just v -> Left (v, m)

{--
  Handles the special keyword define
--}
defineSymbol :: [Ast] -> VarMap -> Either (String, Ast) String
defineSymbol [Sym s, v] m = case evalAst v m of
  Right msg -> Right msg
  Left (None, _) -> Right "Can't define symbol to None"
  Left (eo, _) -> Left (s, eo)
defineSymbol [s, _] _ = Right $ "Can only define a symbol, not " ++ show s
defineSymbol _ _ = Right "define takes exactly two argument"

{--
  evaluates an Ast and also update variables if there's a define
  on error returns a Right string with a description of the error
  on success returns on the left a tuple with the evaluated expression and new map
--}
evalAst :: Ast -> VarMap -> Either (Ast, VarMap) String
evalAst (Value i) m = Left (Value i, m)
evalAst (Sym s) m = tryReadVar s m
evalAst (Call o l) m = evalCall o l m
evalAst (Lambda x) m = Left (Lambda x, m)
evalAst None m = Left (None, m)
evalAst (Boolean b) m = Left (Boolean b, m)

{--
    evalLambda [ArgName1, ArgName2, ...] Expression ArgList
--}
evalLambda :: [String] -> Ast -> [Ast] -> VarMap -> Either Ast String
evalLambda argsName expr args vars
  | length argsName /= length args = Right "Incorrect number of arguments"
  | otherwise = case evalAst expr (insertLambdaVars argsName args vars) of
      Right message -> Right message
      Left (retVal, _) -> Left retVal

{--
    Inserts a new element in a map
--}
insertLambda :: String -> Ast -> VarMap -> VarMap
insertLambda = Map.insert

{--
    Inserts multiple new elements in a map
--}
insertLambdaVars :: [String] -> [Ast] -> VarMap -> VarMap
insertLambdaVars (name : names) (var : vars) vmap = insertLambda name var nmap
  where
    nmap = insertLambdaVars names vars vmap
insertLambdaVars _ _ m = m

{--
    createLambda Name [ArgName1, ArgName2, ..., ArgNameN] Expression
--}
createLambda :: [String] -> Ast -> Ast
createLambda args ast = Lambda (evalLambda args ast)
