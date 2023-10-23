{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}

module Vm
    ( exec
    ) where

import Data.Int (Int64)

data Value = Nb Int
    | Boolean Bool
    deriving (Show, Eq)

data Symbol = Val Value
    | Func [Instruction]

data Op = Addition
    | Substraction
    | Multiplication
    | Division
    | Equal
    | Less
    deriving Show

data Instruction = Function String [String]
    | Push Value
    | PushSymbol String
    | JIF Int64
    | Jump Int64
    | Call
    | Set
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | AND
    | OR
    | NOT
    | EQ
    | NEQ
    | LT
    | LET
    | GT
    | GET
    | NEGATIVE
    | TERNARY
    | RET
    | LIST Int64
    | INDEX String
    deriving (Show, Eq)

type Stack = [Value]
type Insts = [Instruction]

exec :: Insts -> Stack -> [(String, Symbol)] -> (Value, Stack)
exec ((Push val):l) s v = exec l (push s val) v
exec (ADD:l) s v = exec l (opStack s opAdd) v
exec (SUB:l) s v = exec l (opStack s opSub) v
exec (MUL:l) s v = exec l (opStack s opMul) v
exec (DIV:l) s v = exec l (opStack s opDiv) v
exec (Vm.EQ:l) s v = exec l (opStack s opEq) v
exec (Vm.LT:l) s v = exec l (opStack s opLess) v
exec (RET:l) s v = pop s
exec [] s v = pop s

push :: Stack -> Value -> Stack
push s val = val : s

pop :: Stack -> (Value, Stack)
pop [] = (Nb 0, [])
pop (x:xs) = (x, xs)

opStack :: Stack -> (Value -> Value -> Value) -> Stack
opStack s op = case pop s of
    (v1, tmp_stack) -> case pop tmp_stack of
        (v2, final_stack) -> push final_stack (op v1 v2)

-- jumpIfFalse :: Insts -> Int -> Insts
-- jumpIfFalse [] _ = []
-- jumpIfFalse 

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
opDiv (Nb a) (Nb b) = Nb (a * b)
opDiv (Boolean a) (Boolean b) = Boolean (a && b)

opEq :: Value -> Value -> Value
opEq (Nb a) (Nb b) = Boolean (a == b)
opEq (Boolean a) (Boolean b) = Boolean (a == b)

opLess :: Value -> Value -> Value
opLess (Nb a) (Nb b) = Boolean (a < b)
opLess (Boolean a) (Boolean b) = Boolean (not a && b)
