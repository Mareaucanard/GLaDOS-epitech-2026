{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}

module Vm
    ( exec
    ) where

data Value = Nb Int
    | Boolean Bool
    deriving Show

data Op = Addition Int Int
    | Substraction Int Int
    | Multiplication Int Int
    | Division Int Int
    deriving Show

data Instructions = Push Value
    | Call Op
    | Ret

type Stack = [Value]
type Insts = [Instructions]

exec :: Insts -> Stack -> (Value, Stack)
exec ((Push val)::l) s = exec l (push val)
exec ((Call Addition)::l) s = exec l (opStack s (+))
exec ((Call Substraction)::l) s = exec l (opStack s (-))
exec ((Call Multiplication)::l) s = exec l (opStack s (*))
exec ((Call Division)::l) s = exec l (opStack s (/))
exec (Ret r::l) s = pop s

push :: Stack -> Value -> Stack
push s val = val : s

pop :: Stack -> (Stack, Value)
pop [] = ([], 0)
pop (x:xs) = (xs, x)

opStack :: Stack -> (Int -> Int -> Int) -> Stack
opStack s op = case pop s of
    (tmp_stack, v1) -> case pop tmp_stack of
        (final_stack, v2) -> push final_stack (op v1 v2)
