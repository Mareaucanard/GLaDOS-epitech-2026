{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}

module Vm
    ( exec,
      Symbol(..),
      Stack
    ) where

import qualified Data.Map.Lazy as Map
import Data.Int()
import Types

data Symbol = Val Value
    | Func [String] [Instruction]
    deriving (Show, Eq)

type Stack = [Value]
type Insts = [Instruction]

exec :: Insts -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)
exec ((Push val):l) s vTab past = exec l (push s val) vTab (Push val:past)
exec (ADD:l) s vTab past = do
    s' <- opStack s opAdd
    exec l s' vTab (ADD:past)
exec (SUB:l) s vTab past = do
    s' <- opStack s opSub
    exec l s' vTab (SUB:past)
exec (MUL:l) s vTab past = do
    s' <- opStack s opMul
    exec l s' vTab (MUL:past)
exec (DIV:l) s vTab past = do
    s' <- opStack s opDiv
    exec l s' vTab (DIV:past)
exec (Types.EQ:l) s vTab past = do
    s' <- opStack s opEq
    exec l s' vTab (Types.EQ:past)
exec (NEQ:l) s vTab past = do
    s' <- opStack s opNeq
    exec l s' vTab (NEQ:past)
exec (Types.LT:l) s vTab past = do
    s' <- opStack s opLess
    exec l s' vTab (Types.LT:past)
exec (Types.LET:l) s vTab past = do
    s' <- opStack s opLessEq
    exec l s' vTab (Types.LET:past)
exec (Types.GT:l) s vTab past = do
    s' <- opStack s opGreater
    exec l s' vTab (Types.GT:past)
exec (Types.GET:l) s vTab past = do
    s' <- opStack s opGreaterEq
    exec l s' vTab (GET:past)
exec (MOD:l) s vTab past = do
    s' <- opStack s opMod
    exec l s' vTab (MOD:past)
exec (AND:l) s vTab past = do
    s' <- opStack s opAnd
    exec l s' vTab (AND:past)
exec (OR:l) s vTab past = do
    s' <- opStack s opOr
    exec l s' vTab (OR:past)
exec (NOT:l) s vTab past = do
    s' <- singleOpStack s opNot
    exec l s' vTab (NOT:past)
exec (NEGATIVE:l) s vTab past = do
    s' <- singleOpStack s opNeg
    exec l s' vTab (NEGATIVE:past)
exec (TERNARY:l) s vTab past = exec l (opTernary s) vTab (TERNARY:past)
exec ((JIF jmp):l) s vTab past = jumpIfFalse l (fromIntegral jmp) (fromIntegral jmp) s vTab (JIF jmp:past)
exec ((Jump jmp):l) s vTab past = jump l (fromIntegral jmp) (fromIntegral jmp) s vTab (Jump jmp:past)
exec ((LIST n):l) s vTab past = exec l (opList s (fromIntegral n) []) vTab (LIST n:past)
exec (RET:_) s _ _ = return $ pop s
exec [] s _ _ = return $ pop s
exec _ s _ _ = return $ pop s

opStack :: Stack -> (Value -> Value -> Value) -> IO Stack
opStack s op = do
    let (v1, tmp_stack) = pop s
    let (v2, final_stack) = pop tmp_stack
    return (push final_stack (op v1 v2))

singleOpStack :: Stack -> (Value -> Value) -> IO Stack
singleOpStack s op = do 
    let (v1, final_stack) = pop s
    return (push final_stack (op v1))

-- Instructions

push :: Stack -> Value -> Stack
push s val = val:s

pop :: Stack -> (Value, Stack)
pop [] = (Integer 0, [])
pop (x:xs) = (x, xs)

jumpIfFalse :: Insts -> Int -> Int -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)
jumpIfFalse [] a a2 b c [] = exec [] b c []
jumpIfFalse (n:next) jmp j2 (Boolean True:stk) vars prev = exec next stk vars (n:prev)
jumpIfFalse l 0 _ b c prev = exec l b c prev
jumpIfFalse (n:next) jmp j2 [] vars (p:prev) = return (Integer 0, [])
jumpIfFalse (n:next) jmp j2 (s:stk) vars (p:prev) | jmp > length (n:next) && jmp /= j2 = exec [RET] stk vars prev
                                   | jmp < length (p:prev) && jmp /= j2 = exec [RET] stk vars prev
                                   | jmp > 0 = jumpIfFalse next (jmp-1) j2 stk vars (n:p:prev)
                                   | jmp < 0 = jumpIfFalse (p:n:next) (jmp+1) j2 stk vars prev

jump :: Insts -> Int -> Int -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)
jump [] a a2 b c [] = exec [] b c []
jump l 0 _ b c prev = exec l b c prev
jump (n:next) jmp j2 [] vars (p:prev) = return (Integer 0, [])
jump (n:next) jmp j2 stk vars (p:prev) | jmp > length (n:next) && jmp /= j2 = exec [Push (Integer (-9999)), RET] stk vars prev
                                   | jmp < length (p:prev) && jmp /= j2 = exec [Push (Integer (-9999)), RET] stk vars prev
                                   | jmp > 0 = jump next (jmp-1) j2 stk vars (n:p:prev)
                                   | jmp < 0 = jump (p:n:next) (jmp+1) j2 stk vars prev

opAdd :: Value -> Value -> Value
opAdd (Integer a) (Integer b) = Integer (a + b)
opAdd (Boolean a) (Boolean b) = Boolean (a || b)
opAdd (Str a) (Str b) = Str (a ++ b)

opSub :: Value -> Value -> Value
opSub (Integer a) (Integer b) = Integer (a - b)
opSub (Boolean a) (Boolean b) = Boolean (a && not b)

opMul :: Value -> Value -> Value
opMul (Integer a) (Integer b) = Integer (a * b)
opMul (Boolean a) (Boolean b) = Boolean (a && b)

opDiv :: Value -> Value -> Value
opDiv (Integer a) (Integer b) = Integer (a * b)
opDiv (Boolean a) (Boolean b) = Boolean (a && b)

opEq :: Value -> Value -> Value
opEq (Integer a) (Integer b) = Boolean (a == b)
opEq (Boolean a) (Boolean b) = Boolean (a == b)
opEq (Char a) (Char b) = Boolean (a == b)
opEq (Str a) (Str b) = Boolean (a == b)
opEq _ _ = Boolean False

opNeq :: Value -> Value -> Value
opNeq a b = case opEq a b of
    Boolean result -> Boolean (not result)

opLess :: Value -> Value -> Value
opLess (Integer a) (Integer b) = Boolean (a < b)
opLess (Boolean a) (Boolean b) = Boolean (not a && b)

opLessEq :: Value -> Value -> Value
opLessEq (Integer a) (Integer b) = Boolean (a <= b)
opLessEq (Boolean a) (Boolean b) = Boolean ((not a && b) || (a && b))

opGreater :: Value -> Value -> Value
opGreater (Integer a) (Integer b) = Boolean (a > b)
opGreater (Boolean a) (Boolean b) = Boolean (a && not b)

opGreaterEq :: Value -> Value -> Value
opGreaterEq (Integer a) (Integer b) = Boolean (a >= b)
opGreaterEq (Boolean a) (Boolean b) = Boolean ((a && not b) || (a && b))

opMod :: Value -> Value -> Value
opMod (Integer a) (Integer b) = Integer (a `mod` b)

opAnd :: Value -> Value -> Value
opAnd (Boolean a) (Boolean b) = Boolean (a && b)

opOr :: Value -> Value -> Value
opOr (Boolean a) (Boolean b) = Boolean (a || b)

opNot :: Value -> Value
opNot (Boolean a) = Boolean (not a)

opNeg :: Value -> Value
opNeg (Boolean a) = opNot (Boolean a)
opNeg (Integer a) = Integer (a * (-1))
opNeg (Float a) = Float (a * (-1))

opTernary :: Stack -> Stack
opTernary s = do
    let (condition, tmp_stack) = pop s
    let (trueVal, tmp_stack2) = pop tmp_stack
    let (falseVal, final_stack) = pop tmp_stack2
    case condition of
        Boolean True -> push final_stack trueVal
        Boolean False -> push final_stack falseVal

opList :: Stack -> Int -> [Value] -> Stack
opList stk 0 vals = ListVM vals:stk
opList [] _ vals = [ListVM vals]
opList (s:stk) n vals = opList stk (n-1) (vals ++ [s])

-- BUILTINS

opPrintList :: [Value] -> Bool -> IO ()
opPrintList [] _ = putStr "]"
opPrintList list True = putStr "[" >> opPrintList list False
opPrintList (x:xs) False
    | (length (x:xs)) == 1 = opPrintValue x >> opPrintList xs False
    | otherwise = opPrintValue x >> putStr ", " >> opPrintList xs False

opPrintValue :: Value -> IO ()
opPrintValue (Str s) = putStrLn s
opPrintValue (Integer i) = putStrLn (show i)
opPrintValue (Float f) = putStrLn (show f)
opPrintValue (Boolean b) = putStrLn (show b)
opPrintValue (Char c) = putChar c
opPrintValue (ListVM l) = opPrintList l True
opPrintValue (SymVM s) = putStrLn (show s)
opPrintValue Nil = putStrLn "Nil"

opPrint :: Stack -> IO (Value, Stack)
opPrint ((Str s):stk) = do
                        opPrintValue (Str s)
                        return (Str s, stk)
opPrint ((Integer i):stk) = do
                        opPrintValue (Integer i)
                        return (Integer i, stk)
opPrint ((Float f):stk) = do
                        opPrintValue (Float f)
                        return (Float f, stk)
opPrint ((Boolean b):stk) = do
                        opPrintValue (Boolean b)
                        return (Boolean b, stk)
opPrint ((Char c):stk) = do
                        opPrintValue (Char c)
                        return (Char c, stk)
opPrint ((ListVM l):stk) = do
                        opPrintValue (ListVM l)
                        return (ListVM l, stk)
opPrint ((SymVM s):stk) = do
                        opPrintValue (SymVM s)
                        return (SymVM s, stk)
opPrint (Nil:stk) = do
                        opPrintValue Nil
                        return (Nil, stk)


