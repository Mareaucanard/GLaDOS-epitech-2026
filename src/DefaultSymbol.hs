module DefaultSymbol (defaultSymbols) where

import Types (Ast (..), VarMap)
import qualified Data.Map.Lazy as Map

-- |Addition operation.
addAst :: [Ast] -> VarMap -> Either Ast String
addAst [Value a, Value b] _ = Left (Value (a + b))
addAst _ _ = Right "Invalid use of add operation"

-- |Subtraction operation.
subAst :: [Ast] -> VarMap -> Either Ast String
subAst [Value a, Value b] _ = Left (Value (a - b))
subAst _ _ = Right "Invalid use of sub operation"

-- |Multiplication operation.
mulAst :: [Ast] -> VarMap -> Either Ast String
mulAst [Value a, Value b] _ = Left (Value (a * b))
mulAst _ _ = Right "Invalid use of times operation"

-- |Division operation.
divAst :: [Ast] -> VarMap -> Either Ast String
divAst [Value _, Value 0] _ = Right "Division by zero"
divAst [Value a, Value b] _ = Left (Value (a `quot` b))
divAst _ _ = Right "Invalid use of div operation"

-- |Modulo operation.
modAst :: [Ast] -> VarMap -> Either Ast String
modAst [Value _, Value 0] _ = Right "Module by zero"
modAst [Value a, Value b] _ = Left (Value (a `mod` b))
modAst _ _ = Right "Invalid use of mod operation"

-- |Power operation.
powAst :: [Ast] -> VarMap -> Either Ast String
powAst [Value a, Value b] _ = Left (Value (a ^ b))
powAst _ _ = Right "Invalid use of power operation"

-- |Default symbols.
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
