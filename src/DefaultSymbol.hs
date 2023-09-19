module DefaultSymbol (defaultSymbols) where

import qualified Data.Map.Lazy as Map
import Ast (Ast (..), VarMap (..))

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
