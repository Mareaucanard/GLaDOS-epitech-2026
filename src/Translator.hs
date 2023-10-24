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
import Vm (exec)

makeSym :: [Instruction] -> Map.Map String Symbol -> Map.Map String Symbol
makeSym [] _ = []
makeSym ((Function name insts):xs) m =
    let updatedMap = Map.insert name insts m
    in makeSym xs updatedMap

removeFunc :: [Instruction] -> [Instruction]
removeFunc [] = []
removeFunc ((Function "main" insts):xs) = insts
removeFunc _ = removeFunc xs

-- exec :: Insts -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)

translate :: [Instruction] -> [Instruction]
translate insts = exec (removeFunc inst) [] (makeSym (inst Map.empty)) []