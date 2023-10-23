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

exec :: Insts -> Stack -> [(String, Symbol)] -> Insts -> (Value, Stack)
exec ((Push val):l) s vTab past = exec l (push s val) vTab (Push val:past)
exec (ADD:l) s vTab past = exec l (opStack s opAdd) vTab (ADD:past)
exec (SUB:l) s vTab past = exec l (opStack s opSub) vTab (SUB:past)
exec (MUL:l) s vTab past = exec l (opStack s opMul) vTab (MUL:past)
exec (DIV:l) s vTab past = exec l (opStack s opDiv) vTab (DIV:past)
exec (Vm.EQ:l) s vTab past = exec l (opStack s opEq) vTab (Vm.EQ:past)
exec (Vm.LT:l) s vTab past = exec l (opStack s opLess) vTab (Vm.LT:past)
exec ((JIF jmp):l) s vTab past = jumpIfFalse (JIF jmp:l) (fromIntegral jmp) s vTab past
exec (RET:l) s _ _ = pop s
exec [] s _ _ = pop s

push :: Stack -> Value -> Stack
push s val = val:s

pop :: Stack -> (Value, Stack)
pop [] = (Nb 0, [])
pop (x:xs) = (x, xs)

opStack :: Stack -> (Value -> Value -> Value) -> Stack
opStack s op = case pop s of
    (v1, tmp_stack) -> case pop tmp_stack of
        (v2, final_stack) -> push final_stack (op v1 v2)

jumpIfFalse :: Insts -> Int -> Stack -> [(String, Symbol)] -> Insts -> (Value, Stack)
jumpIfFalse [] a b c [] = exec [] b c []
jumpIfFalse (n:next) jmp (Boolean True:stk) vars prev = exec next stk vars (n:prev)
jumpIfFalse (s:l) 0 b c prev = exec l b c (s:prev)
jumpIfFalse (n:next) jmp [] vars (p:prev) = (Nb 0, [])
jumpIfFalse (n:next) jmp (s:stk) vars (p:prev) | jmp > length (n:next) = exec [RET] stk vars prev
                                   | jmp < length (p:prev) = exec [RET] stk vars prev
                                   | jmp > 0 = jumpIfFalse next (jmp-1) stk vars (n:p:prev)
                                   | jmp < 0 = jumpIfFalse (p:n:next) (jmp+1) stk vars prev

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
