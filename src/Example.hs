module Example where

import Ast (Ast (..), VarMap)

-- | Magic [ArgName1, ArgName2, ...] Expression ArgList
magic :: [String] -> Ast -> [Ast] -> VarMap -> Ast
magic argsName expr args vars = Value 0

-- | CreateLambda Name [ArgName1, ArgName2, ..., ArgNameN] Expression
createLambda :: [String] -> Ast -> ([Ast] -> VarMap -> Ast)
createLambda args ast = magic args ast
