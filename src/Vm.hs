{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}
{-# LANGUAGE InstanceSigs #-}

module Vm (exec, Symbol(..), Stack, builtInMap) where

import qualified Data.Map.Lazy as Map
import           Data.Int (Int64)
import           Types
import           Data.List (intercalate)
import           System.Random (randomIO)
import           System.IO (stderr, openFile, IOMode(WriteMode, ReadMode)
                          , hPutStr, hGetLine, hClose, hIsEOF, hPutStrLn)

data Symbol = Val Value
            | Func [String] [Instruction]
            | BuiltIn (Stack -> VarMap -> IO (Value, Stack))

instance Show Symbol where
  show :: Symbol -> String
  show (Val v) = show v
  show (Func args insts) = show args ++ show insts
  show (BuiltIn _) = show "built-in"

type Stack = [Value]

type Insts = [Instruction]

type VarMap = Map.Map String Symbol

insertArgs :: [String] -> Stack -> VarMap -> (Stack, VarMap)
insertArgs [] s m = (s, m)
insertArgs (x:xs) s m = (child_s, Map.insert x (Val val) child_m)
  where
    (child_s, child_m) = insertArgs xs new_stack m

    (val, new_stack) = pop s

call :: Stack -> VarMap -> IO (Value, Stack)
call stack var_map = case pop stack of
  (SymVM func_name, tmp_stack) -> case Map.lookup func_name var_map of
    Nothing -> return (Nil, tmp_stack)
    Just (Val _) -> return (Nil, tmp_stack)
    Just (BuiltIn f) -> f tmp_stack var_map
    Just (Func args insts) -> exec_call
      >>= (\(rval, _) -> return (rval, final_stack))
      where
        (final_stack, new_var_map) = insertArgs args tmp_stack var_map

        exec_call = exec insts [] new_var_map []
  (_, tmp_stack) -> return (Nil, tmp_stack)

fi :: Int64 -> Int
fi = fromIntegral

exec :: Insts -> Stack -> Map.Map String Symbol -> Insts -> IO (Value, Stack)
exec ((Push val):l) s vTab past = exec l (push s val) vTab (Push val:past)
exec (Print:l) s vTab past = do
  (val, s') <- opPrint s
  exec l s' vTab (Print:past)
exec (Set:l) s vTab past = do
  (nVTab, s') <- set s vTab
  exec l s' nVTab (Set:past)
exec ((PushSymbol sym):l) s vTab past =
  exec l (pushSym s sym) vTab (PushSymbol sym:past)
exec (Call:l) s vTab past = call s vTab
  >>= (\(v, new_s) -> exec l (push new_s v) vTab (Call:past))
exec ((INDEX nomVar):l) s vTab past =
  exec l (index s nomVar) vTab (INDEX nomVar:past)
exec (ADD:l) s vTab past = do
  s' <- opStack s opAdd vTab
  exec l s' vTab (ADD:past)
exec (SUB:l) s vTab past = do
  s' <- opStack s opSub vTab
  exec l s' vTab (SUB:past)
exec (MUL:l) s vTab past = do
  s' <- opStack s opMul vTab
  exec l s' vTab (MUL:past)
exec (DIV:l) s vTab past = do
  s' <- opStack s opDiv vTab
  exec l s' vTab (DIV:past)
exec (Types.EQ:l) s vTab past = do
  s' <- opStack s opEq vTab
  exec l s' vTab (Types.EQ:past)
exec (NEQ:l) s vTab past = do
  s' <- opStack s opNeq vTab
  exec l s' vTab (NEQ:past)
exec (Types.LT:l) s vTab past = do
  s' <- opStack s opLess vTab
  exec l s' vTab (Types.LT:past)
exec (Types.LET:l) s vTab past = do
  s' <- opStack s opLessEq vTab
  exec l s' vTab (Types.LET:past)
exec (Types.GT:l) s vTab past = do
  s' <- opStack s opGreater vTab
  exec l s' vTab (Types.GT:past)
exec (Types.GET:l) s vTab past = do
  s' <- opStack s opGreaterEq vTab
  exec l s' vTab (GET:past)
exec (MOD:l) s vTab past = do
  s' <- opStack s opMod vTab
  exec l s' vTab (MOD:past)
exec (AND:l) s vTab past = do
  s' <- opStack s opAnd vTab
  exec l s' vTab (AND:past)
exec (OR:l) s vTab past = do
  s' <- opStack s opOr vTab
  exec l s' vTab (OR:past)
exec (NOT:l) s vTab past = do
  s' <- singleOpStack s opNot
  exec l s' vTab (NOT:past)
exec (NEGATIVE:l) s vTab past = do
  s' <- singleOpStack s opNeg
  exec l s' vTab (NEGATIVE:past)
exec (TERNARY:l) s vTab past = exec l (opTernary s) vTab (TERNARY:past)
exec ((JIF jmp):l) s vTab past = jif (JIF jmp:l) (fi jmp) s vTab past
exec ((Jump jmp):l) s vTab past = jump (Jump jmp:l) (fi jmp) s vTab past
exec ((LIST n):l) s vTab past = exec l (opList s (fi n) []) vTab (LIST n:past)
exec (RET:_) s _ _ = return $ pop s
exec [] s _ _ = return $ pop s
exec _ s _ _ = return $ pop s

opStack :: Stack -> (Value -> Value -> Value) -> VarMap -> IO Stack
opStack s op m = do
  let (v1, tmp_stack) = pop s
  let (v2, final_stack) = pop tmp_stack
  return (push final_stack (op (flatten v1 m) (flatten v2 m)))

singleOpStack :: Stack -> (Value -> Value) -> IO Stack
singleOpStack s op = do
  let (v1, final_stack) = pop s
  return (push final_stack (op v1))

-- Instructions
pushSym :: Stack -> String -> Stack
pushSym s sym = SymVM sym:s

push :: Stack -> Value -> Stack
push s val = val:s

pop :: Stack -> (Value, Stack)
pop [] = (Integer 0, [])
pop (x:xs) = (x, xs)

-- replace :: Value -> Int -> Value
-- replace (ListVM l) = ListVM l
-- replace v = v
set :: Stack -> Map.Map String Symbol -> IO (Map.Map String Symbol, Stack)
set ((SymVM s):(val):stk) vTab = return (Map.insert s (Val val) vTab, stk)
--set ((Reference ref):(val):stk) vTab = return (Map.updateWithKey replace (second ref) vTab, stk)
set stack vTab = return (vTab, stack)

index :: Stack -> String -> Stack
index (Integer i:s) nomVar = push s (Reference (nomVar, fi i))
index s _ = s

jif :: Insts -> Int -> Stack -> VarMap -> Insts -> IO (Value, Stack)
jif insts n s m past = case popN s m 1 of
  (new_s, [Boolean True]) -> jump insts 0 new_s m past
  (new_s, []) -> jump insts n new_s m past

transfer :: [a] -> [a] -> Int -> Maybe ([a], [a])
transfer x y 0 = Just (x, y)
transfer (x:xs) ys n = transfer xs (x:ys) (n - 1)
transfer [] _ _ = Nothing

getNewInsts :: Insts -> Insts -> Int -> Maybe (Insts, Insts)
getNewInsts insts past n =
  if n >= 0
  then transfer insts past (n + 1)
  else case transfer past insts (-n) of
    Nothing -> Nothing
    Just (new_past, new_in) -> Just (new_in, new_past)

jump :: Insts -> Int -> Stack -> VarMap -> Insts -> IO (Value, Stack)
jump insts n s m past = case getNewInsts insts past n of
  Nothing -> printerr "Invalid jump" >> return (Nil, s)
  Just (new_in, new_past) -> exec new_in s m new_past

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
opEq a b = Boolean (a == b)

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
    Boolean True  -> push final_stack trueVal
    Boolean False -> push final_stack falseVal

opList :: Stack -> Int -> [Value] -> Stack
opList stk 0 vals = ListVM vals:stk
opList [] _ vals = [ListVM vals]
opList (s:stk) n vals = opList stk (n - 1) (vals ++ [s])

-- BUILTINS
flatten :: Value -> VarMap -> Value
flatten (SymVM v) m = case Map.lookup v m of
  Nothing      -> Nil
  Just (Val x) -> flatten x m
  Just _       -> Nil
flatten x _ = x

opPrintList :: [Value] -> Bool -> IO ()
opPrintList [] _ = putStr "]"
opPrintList list True = putStr "[" >> opPrintList list False
opPrintList (x:xs) False
  | (length (x:xs)) == 1 = opPrintValue x >> opPrintList xs False
  | otherwise = opPrintValue x >> putStr ", " >> opPrintList xs False

opPrintValue :: Value -> IO ()
opPrintValue (Str s) = putStr s
opPrintValue (Integer i) = putStr (show i)
opPrintValue (Float f) = putStr (show f)
opPrintValue (Boolean b) = putStr (show b)
opPrintValue (Char c) = putChar c
opPrintValue (ListVM l) = opPrintList l True
opPrintValue (SymVM s) = putStr (show s)
opPrintValue Nil = putStr "Nil"

opPrint :: Stack -> IO (Value, Stack)
opPrint ((Str s):stk) = do
  opPrintValue (Str s) >> putStr "\n"
  return (Str s, stk)
opPrint ((Integer i):stk) = do
  opPrintValue (Integer i) >> putStr "\n"
  return (Integer i, stk)
opPrint ((Float f):stk) = do
  opPrintValue (Float f) >> putStr "\n"
  return (Float f, stk)
opPrint ((Boolean b):stk) = do
  opPrintValue (Boolean b) >> putStr "\n"
  return (Boolean b, stk)
opPrint ((Char c):stk) = do
  opPrintValue (Char c) >> putStr "\n"
  return (Char c, stk)
opPrint ((ListVM l):stk) = do
  opPrintValue (ListVM l) >> putStr "\n"
  return (ListVM l, stk)
opPrint ((SymVM s):stk) = do
  opPrintValue (SymVM s) >> putStr "\n"
  return (SymVM s, stk)
opPrint (Nil:stk) = do
  opPrintValue Nil >> putStr "\n"
  return (Nil, stk)

-- Builtin
type BuiltInFunc = Stack -> VarMap -> IO (Value, Stack)

popN :: Stack -> VarMap -> Int -> (Stack, [Value])
popN s _ 0 = (s, [])
popN [] _ _ = ([], [])
popN (x:xs) m n = (final, flatten x m:t)
  where
    (final, t) = popN xs m (n - 1)

printerr :: String -> IO ()
printerr = hPutStrLn stderr

builtInMap :: VarMap
builtInMap = Map.fromList
  [ ("print", BuiltIn printCall)
  , ("typeOf", BuiltIn typeOfCall)
  , ("rand", BuiltIn rand)
  , ("open", BuiltIn open)
  , ("close", BuiltIn closeCall)
    --   , ("write", BuiltIn writeCall)
  , ("read", BuiltIn readCall)]

typeOfVal :: Value -> String
typeOfVal (Integer _) = "integer"
typeOfVal (Float _) = "float"
typeOfVal (Char _) = "char"
typeOfVal (Boolean _) = "boolean"
typeOfVal (Str _) = "string"
typeOfVal (ListVM _) = "list"
typeOfVal (Nil) = "nil"
typeOfVal (File _) = "file"
typeOfVal (Reference _) = "ref"
typeOfVal (SymVM _) = "sym"

typeOfCall :: BuiltInFunc
typeOfCall s m = case pop s of
  (SymVM sym, final_stack) -> case Map.lookup sym m of
    Just (Val v)     -> putStrLn $ typeOfVal v
    Just (Func _ _)  -> putStrLn "function"
    Just (BuiltIn _) -> putStrLn "function"
    Nothing          -> putStrLn $ typeOfVal Nil
    >> return (Nil, final_stack)
  (x, final_stack)         -> print x >> return (Nil, final_stack)

printCall :: BuiltInFunc
printCall s m = case pop s of
  (SymVM sym, final_stack) -> case Map.lookup sym m of
    Just (Val v) -> print v
    Just (Func args _)
      -> putStrLn $ "Function " ++ sym ++ "(" ++ intercalate ", " args ++ ")"
    Just (BuiltIn _) -> putStrLn $ "Builtin " ++ show sym
    Nothing -> print Nil
    >> return (Nil, final_stack)
  (x, final_stack)         -> print x >> return (Nil, final_stack)

rand :: BuiltInFunc
rand s _ = (randomIO :: IO Double) >>= (\x -> return (Float x, s))

openLogic :: String -> String -> Stack -> IO (Value, Stack)
openLogic filename "w" s =
  openFile filename WriteMode >>= \f -> return (File f, s)
openLogic filename "r" s =
  openFile filename ReadMode >>= \f -> return (File f, s)
openLogic _ _ s = printerr "Open: invalid mode" >> return (Nil, s)

open :: BuiltInFunc
open s m = case popN s m 2 of
  (f_stack, [Str filename, Str mode]) -> openLogic filename mode f_stack
  (f_stack, [n1, n2]) -> printerr
    ("Could not open files, invalid types: "
     ++ typeOfVal n1
     ++ " "
     ++ typeOfVal n2)
    >> return (Nil, f_stack)
  (f_stack, _) -> printerr "Wrong number of arguments" >> return (Nil, f_stack)

readCall :: BuiltInFunc
readCall s m = case popN s m 1 of
  (f_stack, [File f]) -> hIsEOF f
    >>= \empty -> if empty
                  then return (Nil, f_stack)
                  else hGetLine f >>= \line -> return (Str line, f_stack)
  (f_stack, _)        -> printerr "read: not a file" >> return (Nil, f_stack)

closeCall :: BuiltInFunc
closeCall s m = case popN s m 1 of
  (f_stack, [File f]) -> hClose f >> return (Nil, f_stack)
  (f_stack, _)        -> printerr "close: not a file" >> return (Nil, f_stack)
