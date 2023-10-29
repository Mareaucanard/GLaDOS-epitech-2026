{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Translator
-}

module Translator
    ( translate
    ) where

import qualified Data.Map.Lazy as Map
import Types
import Vm

getInsts :: [Instruction] -> [Instruction] -> [Instruction]
getInsts [] funcInsts = funcInsts
getInsts ((Function _ _):_) funcInsts = funcInsts
getInsts (x:xs) funcInsts = getInsts xs (funcInsts ++ [x])

makeSym :: [Instruction] -> Map.Map String Symbol -> Map.Map String Symbol
makeSym [] m = m
makeSym ((Function name args):xs) m =
    let updatedMap = Map.insert name (Func args (getInsts xs [])) m
    in makeSym xs updatedMap
makeSym (_:xs) m = makeSym xs m

goToMain :: [Instruction] -> [Instruction]
goToMain [] = []
goToMain ((Function "main" _):xs) = xs
goToMain (_:xs) = goToMain xs

-- exec :: Insts -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)
translate :: [Instruction] -> ([Instruction], Stack, Map.Map String Symbol, [Instruction])
translate insts = ((goToMain insts), [], (makeSym insts Map.empty), [])