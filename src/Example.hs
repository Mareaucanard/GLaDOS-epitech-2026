module Example where

import Ast (Ast (..), VarMap)

{--
    magic [ArgName1, ArgName2, ...] Expression ArgList
--}
magic :: [String] -> Ast -> [Ast] -> VarMap -> Ast
magic argsName expr args vars = Value 0

{--
    createLambda Name [ArgName1, ArgName2, ..., ArgNameN] Expression
--}
createLambda :: [String] -> Ast -> ([Ast] -> VarMap -> Ast)
createLambda args ast = magic args ast
