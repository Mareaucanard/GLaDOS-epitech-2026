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

makeSym :: [Instruction] -> [(String, Symbol)]
makeSym [] = []
makeSym ((Function name args):xs) = (name, Func xs) : makeSym xs
makeSym ((PushSymbol name):xs) = (name) : makeSym xs

translate :: [Instruction] -> IO()
translate inst = exec inst makeSym (inst )