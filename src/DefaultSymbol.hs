{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- DefaultSymbol
--}
module DefaultSymbol (defaultSymbols) where

import           Ast (evalAst, mapEvalCalls)
import           Types (Ast(..), VarMap)
import qualified Data.Map.Lazy as Map

evalAstNoVars :: Ast -> VarMap -> Either Ast String
evalAstNoVars ast m = case evalAst ast m of
  Right msg     -> Right msg
  Left (val, _) -> Left val

twoOpFunc :: [Ast]
          -> VarMap
          -> String
          -> (Ast -> Ast -> Either Ast String)
          -> Either Ast String
twoOpFunc [ast1, ast2] m name f = case mapEvalCalls [ast1, ast2] m of
  Right msg -> Right msg
  Left ([a, b], _) -> f a b
  _ -> Right $ "Invalid use of " ++ name ++ " operation"
twoOpFunc _ _ name _ = Right $ "Invalid use of " ++ name ++ " operation"

basicOp :: (Int -> Int -> Int) -> (Ast -> Ast -> Either Ast String)
basicOp f (Value a) (Value b) = Left $ Value (f a b)
basicOp _ v1 v2 = Right $ "Invalid operation" ++ show v1 ++ show v2

boolOp :: (Int -> Int -> Bool) -> (Ast -> Ast -> Either Ast String)
boolOp f (Value a) (Value b) = Left $ Boolean (f a b)
boolOp _ v1 v2 = Right $ "Invalid operation" ++ show v1 ++ show v2

-- |Addition operation.
addAst :: [Ast] -> VarMap -> Either Ast String
addAst args m = twoOpFunc args m "add" (basicOp (+))

-- |Subtraction operation.
subAst :: [Ast] -> VarMap -> Either Ast String
subAst args m = twoOpFunc args m "sub" (basicOp (-))

-- |Multiplication operation.
mulAst :: [Ast] -> VarMap -> Either Ast String
mulAst args m = twoOpFunc args m "times" (basicOp (*))

divLogic :: Ast -> Ast -> Either Ast String
divLogic (Value _) (Value 0) = Right "Division by zero"
divLogic (Value a) (Value b) = Left (Value (a `quot` b))
divLogic _ _ = Right "Invalid use of div operation"

-- |Division operation.
divAst :: [Ast] -> VarMap -> Either Ast String
divAst args m = twoOpFunc args m "div" divLogic

modLogic :: Ast -> Ast -> Either Ast String
modLogic (Value _) (Value 0) = Right "Module by zero"
modLogic (Value a) (Value b) = Left (Value (a `mod` b))
modLogic _ _ = Right "Invalid use of mod operation"

-- |Modulo operation.
modAst :: [Ast] -> VarMap -> Either Ast String
modAst args m = twoOpFunc args m "mod" modLogic

-- |Power operation.
powAst :: [Ast] -> VarMap -> Either Ast String
powAst args m = twoOpFunc args m "power" (basicOp (^))

-- |Default symbols.
ifAst :: [Ast] -> VarMap -> Either Ast String
ifAst [cond, x, y] m = case evalAstNoVars cond m of
  Right err -> Right err
  Left (Boolean bool) -> if bool
                         then evalAstNoVars x m
                         else evalAstNoVars y m
  Left _ -> Right "If condition can only evaluate boolean values"
ifAst s _ = Right $ "Invalid use of if condition" ++ show s

equalAst :: [Ast] -> VarMap -> Either Ast String
equalAst args m = twoOpFunc args m "equal" (boolOp (==))

diffAst :: [Ast] -> VarMap -> Either Ast String
diffAst args m = twoOpFunc args m "not equal" (boolOp (/=))

infAst :: [Ast] -> VarMap -> Either Ast String
infAst args m = twoOpFunc args m "lower than" (boolOp (<))

infEqAst :: [Ast] -> VarMap -> Either Ast String
infEqAst args m = twoOpFunc args m "lower or equal than" (boolOp (<=))

supAst :: [Ast] -> VarMap -> Either Ast String
supAst args m = twoOpFunc args m "greater than" (boolOp (>))

supEqAst :: [Ast] -> VarMap -> Either Ast String
supEqAst args m = twoOpFunc args m "greater or equal than than" (boolOp (>=))

list :: [Ast] -> VarMap -> Either Ast String
list [] _ = Left (Tab [])
list (x:xs) m = case evalAst x m of
  Right msg1    -> Right msg1
  Left (val, _) -> case list xs m of
    Right msg2 -> Right msg2
    Left (Tab valTail) -> Left (Tab (val:valTail))
    Left _ -> Right "Unknown error in list function"

car :: [Ast] -> VarMap -> Either Ast String
car [arg] m = case evalAst arg m of
  Right msg           -> Right msg
  Left (Tab [], _)    -> Right "Can't apply car on empty list"
  Left (Tab (x:_), _) -> Left x
  Left (v, _)         -> Right
    $ "Invalid use of car function, only works on lists but got " ++ show v
car l _ = Right
  $ "Invalid argument count for car, expected 1 but got " ++ show (length l)

cdr :: [Ast] -> VarMap -> Either Ast String
cdr [arg] m = case evalAst arg m of
  Right msg -> Right msg
  Left (Tab [], _) -> Right "Can't apply cdr on empty list"
  Left (Tab (_:xs), _) -> Left (Tab xs)
  Left (v, _) -> Right
    $ "Invalid use of cdr function, only works on lists but got " ++ show v
cdr l _ = Right
  $ "Invalid argument count for cdr, expected 1 but got " ++ show (length l)

cons :: [Ast] -> VarMap -> Either Ast String
cons [arg1, arg2] m = case evalAst arg1 m of
  Right msg1   -> Right msg1
  Left (v1, _) -> case evalAst arg2 m of
    Right msg2        -> Right msg2
    Left (Tab lst, _) -> Left (Tab (v1:lst))
    Left (v2, _)      -> Left (Tab [v1, v2])
cons l _ = Right
  $ "Invalid argument count for cdr, expected 1 but got " ++ show (length l)

isEmpty :: [Ast] -> VarMap -> Either Ast String
isEmpty [arg] m = case evalAst arg m of
  Right msg        -> Right msg
  Left (Tab [], _) -> Left (Boolean True)
  Left (Tab _, _)  -> Left (Boolean False)
  Left (v, _)
    -> Right $ "Invalid use of isEmpty, only works on lists but got" ++ show v
isEmpty l _ = Right
  $ "Invalid argument count for isEmpty, expected 1 but got "
  ++ show (length l)

defaultSymbols :: VarMap
defaultSymbols = Map.fromList
  [ ("+", Lambda addAst)
  , ("-", Lambda subAst)
  , ("*", Lambda mulAst)
  , ("/", Lambda divAst)
  , ("%", Lambda modAst)
  , ("^", Lambda powAst)
  , ("if", Lambda ifAst)
  , ("==", Lambda equalAst)
  , ("eq?", Lambda equalAst)
  , ("!=", Lambda diffAst)
  , ("/=", Lambda diffAst)
  , ("=/", Lambda diffAst)
  , ("<", Lambda infAst)
  , ("<=", Lambda infEqAst)
  , (">", Lambda supAst)
  , (">=", Lambda supEqAst)
  , ("list", Lambda list)
  , ("car", Lambda car)
  , ("cdr", Lambda cdr)
  , ("cons", Lambda cons)
  , ("isempty", Lambda isEmpty)
  , ("empty", Tab [])
  , ("nil", None)
  , ("seed", Value 7)]
