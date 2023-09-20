module DefaultSymbol (defaultSymbols) where

import Types (Ast (..), VarMap)
import qualified Data.Map.Lazy as Map

addAst :: [Ast] -> VarMap -> Either Ast String
addAst [Value a, Value b] _ = Left (Value (a + b))
addAst _ _ = Right "Invalid use of add operation"

subAst :: [Ast] -> VarMap -> Either Ast String
subAst [Value a, Value b] _ = Left (Value (a - b))
subAst _ _ = Right "Invalid use of sub operation"

mulAst :: [Ast] -> VarMap -> Either Ast String
mulAst [Value a, Value b] _ = Left (Value (a * b))
mulAst _ _ = Right "Invalid use of times operation"

divAst :: [Ast] -> VarMap -> Either Ast String
divAst [Value _, Value 0] _ = Right "Division by zero"
divAst [Value a, Value b] _ = Left (Value (a `quot` b))
divAst _ _ = Right "Invalid use of div operation"

modAst :: [Ast] -> VarMap -> Either Ast String
modAst [Value _, Value 0] _ = Right "Module by zero"
modAst [Value a, Value b] _ = Left (Value (a `mod` b))
modAst _ _ = Right "Invalid use of mod operation"

powAst :: [Ast] -> VarMap -> Either Ast String
powAst [Value a, Value b] _ = Left (Value (a ^ b))
powAst _ _ = Right "Invalid use of power operation"

ifAst :: [Ast] -> VarMap -> Either Ast String
ifAst [Boolean b, x, y] _ = Left (if b then x else y)
ifAst _ _ = Right "Invalid use of if condition"

equalAst :: [Ast] -> VarMap -> Either Ast String
equalAst [Value a, Value b] _ = Left (Boolean (a == b))
equalAst _ _ = Right "Invalid use of equal operation"

diffAst :: [Ast] -> VarMap -> Either Ast String
diffAst [Value a, Value b] _ = Left (Boolean (a /= b))
diffAst _ _ = Right "Invalid use of different operation"

infAst :: [Ast] -> VarMap -> Either Ast String
infAst [Value a, Value b] _ = Left (Boolean (a < b))
infAst _ _ = Right "Invalid use of inferior operation"

infEqAst :: [Ast] -> VarMap -> Either Ast String
infEqAst [Value a, Value b] _ = Left (Boolean (a <= b))
infEqAst _ _ = Right "Invalid use of inferior or equal operation"

supAst :: [Ast] -> VarMap -> Either Ast String
supAst [Value a, Value b] _ = Left (Boolean (a > b))
supAst _ _ = Right "Invalid use of superior operation"

supEqAst :: [Ast] -> VarMap -> Either Ast String
supEqAst [Value a, Value b] _ = Left (Boolean (a >= b))
supEqAst _ _ = Right "Invalid use of superior or equal operation"

defaultSymbols :: VarMap
defaultSymbols =
  Map.fromList
    [ ("+", Lambda addAst),
      ("-", Lambda subAst),
      ("*", Lambda mulAst),
      ("/", Lambda divAst),
      ("%", Lambda modAst),
      ("^", Lambda powAst),
      ("if", Lambda ifAst),
      ("==", Lambda equalAst),
      ("eq?", Lambda equalAst),
      ("!=", Lambda diffAst),
      ("/=", Lambda diffAst),
      ("=/", Lambda diffAst),
      ("<", Lambda infAst),
      ("<=", Lambda infEqAst),
      (">", Lambda supAst),
      (">=", Lambda supEqAst)
    ]
