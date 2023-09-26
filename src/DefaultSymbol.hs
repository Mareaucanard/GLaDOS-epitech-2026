{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- DefaultSymbol
--}

module DefaultSymbol (defaultSymbols) where

import Ast (evalAst, mapEvalCalls)
import Types (Ast (..), VarMap)
import qualified Data.Map.Lazy as Map
import Data.Bits

evalAstNoVars :: Ast -- ^ The first Ast
  -> VarMap -- ^ The map of variables
  -> Either Ast String -- ^ The return value
evalAstNoVars ast m = case evalAst ast m of
  Right msg -> Right msg
  Left (val, _) -> Left val

-- |Function that takes two Ast.
-- If the list of Ast is not of size 2, returns an error.
-- If the list of Ast is of size 2, returns the result of the function f.
twoOpFunc :: [Ast] -- ^ The list of Ast
  -> VarMap -- ^ The map of variables
  -> String -- ^ The name of the operation
  -> (Ast -> Ast -> Either Ast String) -- ^ The function to apply
  -> Either Ast String -- ^ The return value
twoOpFunc [ast1, ast2] m name f = case mapEvalCalls [ast1, ast2] m of
  Right msg -> Right msg
  Left ([a, b], _) -> f a b
  _ -> Right $ "Invalid use of " ++ name ++ " operation"
twoOpFunc _ _ name _ = Right $ "Invalid use of " ++ name ++ " operation"

-- |Basic operation.
-- If the two Ast are not of type Value, returns an error.
-- If the two Ast are of type Value, returns the result of the function f.
basicOp :: (Int -> Int -> Int) -- ^ The function to apply
  -> (Ast -> Ast -> Either Ast String) -- ^ The return value
basicOp f (Value a) (Value b) = Left $ Value (f a b)
basicOp _ v1 v2 = Right $ "Invalid operation " ++ show v1 ++ " " ++ show v2

-- |Boolean operation.
-- If the two Ast are not of type Value, returns an error.
-- If the two Ast are of type Value, returns the result of the function f.
boolOp :: (Int -> Int -> Bool) -> (Ast -> Ast -> Either Ast String)
boolOp f (Value a) (Value b) = Left $ Boolean (f a b)
boolOp _ v1 v2 = Right $ "Invalid operation " ++ show v1 ++ " " ++ show v2

-- |Boolean logic operation.
-- If the two Ast are not of type Boolean, returns an error.
-- If the two Ast are of type Boolean, returns the result of the function f.
logOp :: (Bool -> Bool -> Bool) -> (Ast -> Ast -> Either Ast String)
logOp f (Boolean a) (Boolean b) = Left $ Boolean (f a b)
logOp _ b1 b2 = Right $ "Invalid operation " ++ show b1 ++ " " ++ show b2

-- |Bitwise boolean operation.
-- If the two Ast are not of type Value, returns an error.
-- If the two Ast are of type Value, returns the result of the function f.
binBoolOp :: (Int -> Int -> Int) -> (Ast -> Ast -> Either Ast String)
binBoolOp f (Value a) (Value b) = Left $ Value (f a b)
binBoolOp _ v1 v2 = Right $ "Invalid operation " ++ show v1 ++ " " ++ show v2

-- |Addition operation.
addAst :: [Ast] -> VarMap -> Either Ast String
addAst args m = twoOpFunc args m "add" (basicOp (+))

-- |Subtraction operation.
subAst :: [Ast] -> VarMap -> Either Ast String
subAst args m = twoOpFunc args m "sub" (basicOp (-))

-- |Multiplication operation.
mulAst :: [Ast] -> VarMap -> Either Ast String
mulAst args m = twoOpFunc args m "times" (basicOp (*))

-- |Division operation logic.
divLogic :: Ast -> Ast -> Either Ast String
divLogic (Value _) (Value 0) = Right "Division by zero"
divLogic (Value a) (Value b) = Left (Value (a `quot` b))
divLogic _ _ = Right "Invalid use of div operation"

-- |Division operation.
divAst :: [Ast] -> VarMap -> Either Ast String
divAst args m = twoOpFunc args m "div" divLogic

-- |Modulo operation logic.
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
  Left (Boolean bool) -> if bool then evalAstNoVars x m else evalAstNoVars y m
  Left _ -> Right "If condition can only evaluate boolean values"
ifAst s _ = Right $ "Invalid use of if condition" ++ show s

-- |Equal bool operator.
equalAst :: [Ast] -> VarMap -> Either Ast String
equalAst args m = twoOpFunc args m "equal" (boolOp (==))

-- |Different bool operator.
diffAst :: [Ast] -> VarMap -> Either Ast String
diffAst args m = twoOpFunc args m "unequal" (boolOp (/=))

-- |Inferior bool operator.
infAst :: [Ast] -> VarMap -> Either Ast String
infAst args m = twoOpFunc args m "smaller than" (boolOp (<))

-- |Inferior or equal bool operator.
infEqAst :: [Ast] -> VarMap -> Either Ast String
infEqAst args m = twoOpFunc args m "smaller or equal to" (boolOp (<=))

-- |Superior bool operator.
supAst :: [Ast] -> VarMap -> Either Ast String
supAst args m = twoOpFunc args m "larger than" (boolOp (>))

-- |Superior or equal bool operator.
supEqAst :: [Ast] -> VarMap -> Either Ast String
supEqAst args m = twoOpFunc args m "larger or equal to" (boolOp (>=))

-- |And bool operator
andAst :: [Ast] -> VarMap -> Either Ast String
andAst args m = twoOpFunc args m "and" (logOp(&&))

-- |Or bool operator
orAst :: [Ast] -> VarMap -> Either Ast String
orAst args m = twoOpFunc args m "or" (logOp(||))

-- |Not bool operator
notAst :: [Ast] -> VarMap -> Either Ast String
notAst [ast1] m = case mapEvalCalls [ast1] m of
  Right msg -> Right msg
  Left ([Boolean a], _) -> Left (Boolean (not a))
  _ -> Right "Invalid use of not operation"
notAst _ _ = Right "Invalid use of not operation"

-- |bitwise and operator
binAndAst :: [Ast] -> VarMap -> Either Ast String
binAndAst args m = twoOpFunc args m "&" (binBoolOp (.&.))

-- |bitwise or operator
binOrAst :: [Ast] -> VarMap -> Either Ast String
binOrAst args m = twoOpFunc args m "&" (binBoolOp (.|.))

-- |bitwise not operator
binNotAst :: [Ast] -> VarMap -> Either Ast String
binNotAst [ast1] m = case mapEvalCalls [ast1] m of
  Right msg -> Right msg
  Left ([Value a], _) -> Left (Value (complement a))
  _ -> Right "Invalid use of not operation"
binNotAst _ _ = Right "Invalid use of not operation"

-- |bitwise xor operator
binXorAst :: [Ast] -> VarMap -> Either Ast String
binXorAst args m = twoOpFunc args m "&" (binBoolOp xor)

-- |Default symbols.
defaultSymbols :: VarMap
defaultSymbols =
  Map.fromList
    [ ("+", Lambda addAst),
      ("-", Lambda subAst),
      ("*", Lambda mulAst),
      ("div", Lambda divAst),
      ("mod", Lambda modAst),
      ("**", Lambda powAst),
      ("if", Lambda ifAst),
      ("==", Lambda equalAst),
      ("eq?", Lambda equalAst),
      ("!=", Lambda diffAst),
      ("/=", Lambda diffAst),
      ("=/", Lambda diffAst),
      ("<", Lambda infAst),
      ("<=", Lambda infEqAst),
      (">", Lambda supAst),
      (">=", Lambda supEqAst),
      ("and", Lambda andAst),
      ("or", Lambda orAst),
      ("not", Lambda notAst),
      ("&", Lambda binAndAst),
      ("|", Lambda binOrAst),
      ("^", Lambda binXorAst),
      ("~", Lambda binNotAst)
    ]
