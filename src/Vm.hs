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

data Op = Addition
    | Substraction
    | Multiplication
    | Division
    deriving Show

data Instructions = Push Value
    | Call Op
    | Ret

type Stack = [Value]
type Insts = [Instructions]

exec :: Insts -> Stack -> (Value, Stack)
exec ((Push val):l) s = exec l (push s val)
exec ((Call Addition):l) s = exec l (opStack s opAdd)
exec ((Call Substraction):l) s = exec l (opStack s opSub)
exec ((Call Multiplication):l) s = exec l (opStack s opMul)
exec ((Call Division):l) s = exec l (opStack s opDiv)
exec (Ret:l) s = pop s
exec [] s = pop s

push :: Stack -> Value -> Stack
push s val = val : s

pop :: Stack -> (Value, Stack)
pop [] = (Nb 0, [])
pop (x:xs) = (x, xs)

opStack :: Stack -> (Value -> Value -> Value) -> Stack
opStack s op = case pop s of
    (v1, tmp_stack) -> case pop tmp_stack of
        (v2, final_stack) -> push final_stack (op v1 v2)

opAdd :: Value -> Value -> Value
opAdd (Nb a) (Nb b) = Nb (a + b)
opAdd (Boolean a) (Boolean b) = Boolean (a || b)

opSub :: Value -> Value -> Value
opSub (Nb a) (Nb b) = Nb (a - b)
opSub (Boolean a) (Boolean b) = Boolean (a && not b)

opMul :: Value -> Value -> Value
opMul (Nb a) (Nb b) = Nb (a * b)
opMul (Boolean a) (Boolean b) = Boolean (a && b)

opDiv :: Value -> Value -> Value
opMul (Nb a) (Nb b) = Nb (a * b)
opMul (Boolean a) (Boolean b) = Boolean (a && b)
