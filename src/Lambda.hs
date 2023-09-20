module Lambda (createLambda) where

import Ast (Ast (..), VarMap(..), evalAst)
import qualified Data.Map.Lazy as Map

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
insertLambdaVars (name:names) (var:vars) vmap = insertLambda name var nmap
    where nmap = insertLambdaVars names vars vmap
insertLambdaVars _ _ m = m 

{--
    createLambda Name [ArgName1, ArgName2, ..., ArgNameN] Expression
--}
createLambda :: String -> [String] -> Ast -> Ast
createLambda name args ast = Lambda name (evalLambda args ast)
