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
                          , hGetLine, hClose, hIsEOF, hPutStrLn, stdin, Handle)
import           System.Exit (exitFailure, exitWith, ExitCode(ExitFailure), die)

data Symbol = Val FlatStack
            | Func [String] [Instruction]
            | BuiltIn (Stack -> VarMap -> IO (StackValue, Stack))

instance Show Symbol where
  show :: Symbol -> String
  show (Val v) = show v
  show (Func args insts) = show args ++ show insts
  show (BuiltIn _) = show "built-in"

data FlatStack = V Value
               | Tab [FlatStack]
               | File Handle
  deriving (Show, Eq)

data StackValue = Flat FlatStack
                | SymVM String
                | Ref String StackValue
  deriving (Show)

type Stack = [StackValue]

type Insts = [Instruction]

type VarMap = Map.Map String Symbol

insertArgs :: [String] -> [FlatStack] -> VarMap -> VarMap
insertArgs [] [] m = m
insertArgs (x:xs) (y:ys) m = Map.insert x (Val y) (insertArgs xs ys m)
insertArgs _ _ m = m

call :: Stack -> VarMap -> IO (StackValue, Stack)
call stack m = case pop stack of
  (SymVM func_name, tmp_stack) -> case Map.lookup func_name m of
    Nothing -> die $ "No function named " ++ show func_name
    Just (Val _) -> die $ show func_name ++ " is not a function"
    Just (BuiltIn f) -> f tmp_stack m
    Just (Func args insts) -> case popN tmp_stack m (length args) of
      Left e -> die e
      Right (final_stack, l) -> if length l /= length args
                                then die "Not enough arguments for function"
                                else f
        where
          f = exec insts final_stack (insertArgs args l m) []
            >>= \(rval, _) -> return (rval, final_stack)
  (_, _) -> die "Bad function call, not a function"

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

exec
  :: Insts -> Stack -> Map.Map String Symbol -> Insts -> IO (StackValue, Stack)
exec ((Push val):l) s vTab past = exec l (push s val) vTab (Push val:past)
exec (Set:l) s vTab past = do
  (nVTab, s') <- set s vTab l past
  exec l s' nVTab (Set:past)
exec ((PushSymbol sym):l) s vTab past =
  exec l (pushSym s sym) vTab (PushSymbol sym:past)
exec (Call:l) s vTab past = call s vTab
  >>= (\(v, new_s) -> exec l (v:new_s) vTab (Call:past))
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
  s' <- singleOpStack s opNot vTab
  exec l s' vTab (NOT:past)
exec (NEGATIVE:l) s vTab past = do
  s' <- singleOpStack s opNeg vTab
  exec l s' vTab (NEGATIVE:past)
exec ((JIF jmp):l) s vTab past = jif (JIF jmp:l) (fi jmp) s vTab past
exec ((Jump jmp):l) s vTab past = jump (Jump jmp:l) (fi jmp) s vTab past
exec ((LIST n):l) s vTab past = case opList s vTab (fi n) of
  Left e          -> die e
  Right new_stack -> exec l new_stack vTab (LIST n:past)
exec (RET:_) s _ _ = return $ pop s
exec [] s _ _ = return $ pop s
exec _ s _ _ = return $ pop s

opStack
  :: Stack -> (FlatStack -> FlatStack -> Maybe FlatStack) -> VarMap -> IO Stack
opStack s op m = case popN s m 2 of
  Right (final_stack, [v1, v2]) -> case op v1 v2 of
    Nothing -> die "Invalid types for operation"
    Just v  -> return $ Flat v:final_stack
  Left e -> die e
  _ -> die "STACK ERROR"

singleOpStack :: Stack -> (FlatStack -> Maybe FlatStack) -> VarMap -> IO Stack
singleOpStack s op m = case popN s m 1 of
  Right (final_stack, [v1]) -> case op v1 of
    Just v -> return $ Flat v:final_stack
    _      -> die "Invalid type for operation"
  Left e -> die e
  _ -> die "STACK ERROR"

-- Instructions
pushSym :: Stack -> String -> Stack
pushSym s sym = SymVM sym:s

push :: Stack -> Value -> Stack
push s val = Flat (V val):s

pop :: Stack -> (StackValue, Stack)
pop [] = (Flat (V Nil), [])
pop (x:xs) = (x, xs)

-- replace :: Value -> Int -> Value
-- replace (ListVM l) = ListVM l
-- replace v = v
set :: Stack -> Map.Map String Symbol -> Insts -> Insts -> IO (VarMap, Stack)
set s m i past = case pop s of
  (SymVM sym, tmp_stack) -> case popN tmp_stack m 1 of
    Right (final_stack, [x]) -> return (Map.insert sym (Val x) m, final_stack)
    Left e -> die e
    _ -> die "STACK ERROR"
  (Flat x, _) -> die
    $ "Invalid stack SET, can't attribute to such type"
    ++ typeOfVal x
    ++ show s
    ++ show i
    ++ show past
  _ -> die "Invalid stack SET"

index :: Stack -> String -> Stack
index s varName = Ref varName val:new_s
  where
    (val, new_s) = pop s

jif :: Insts -> Int -> Stack -> VarMap -> Insts -> IO (StackValue, Stack)
jif insts n s m past = case popN s m 1 of
  Right (new_s, [V (Boolean True)]) -> jump insts 0 new_s m past
  Right (new_s, _) -> jump insts n new_s m past
  Left e -> die e

transfer :: [a] -> [a] -> Int -> Maybe ([a], [a])
transfer x y 0 = Just (x, y)
transfer (x:xs) ys n = transfer xs (x:ys) (n - 1)
transfer [] _ _ = Nothing

getNewInsts :: Insts -> Insts -> Int -> Maybe (Insts, Insts)
getNewInsts insts past n =
  if n >= 0
  then transfer insts past (n + 1)
  else case transfer past insts (-n - 1) of
    Nothing -> Nothing
    Just (new_past, new_in) -> Just (new_in, new_past)

jump :: Insts -> Int -> Stack -> VarMap -> Insts -> IO (StackValue, Stack)
jump insts n s m past = case getNewInsts insts past n of
  Nothing -> die "Invalid jump"
  Just (new_in, new_past) -> exec new_in s m new_past

opAdd :: FlatStack -> FlatStack -> Maybe FlatStack
opAdd (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x + y))
opAdd (V (Float x)) (V (Integer y)) = Just $ V (Float (x + fi y))
opAdd (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x + y))
opAdd _ _ = Nothing

opSub :: FlatStack -> FlatStack -> Maybe FlatStack
opSub (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x - y))
opSub (V (Float x)) (V (Integer y)) = Just $ V (Float (x - fi y))
opSub (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x - y))
opSub _ _ = Nothing

opMul :: FlatStack -> FlatStack -> Maybe FlatStack
opMul (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x * y))
opMul (V (Float x)) (V (Integer y)) = Just $ V (Float (x * fi y))
opMul (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x * y))
opMul _ _ = Nothing

opDiv :: FlatStack -> FlatStack -> Maybe FlatStack
opDiv (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x `div` y))
opDiv (V (Float x)) (V (Integer y)) = Just $ V (Float (x / fi y))
opDiv (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x / y))
opDiv _ _ = Nothing

opMod :: FlatStack -> FlatStack -> Maybe FlatStack
opMod (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x `mod` y))
opMod _ _ = Nothing

opEq :: FlatStack -> FlatStack -> Maybe FlatStack
opEq a b = Just $ V $ Boolean (a == b)

opNeq :: FlatStack -> FlatStack -> Maybe FlatStack
opNeq a b = Just $ V $ Boolean (a /= b)

opLess :: FlatStack -> FlatStack -> Maybe FlatStack
opLess (V (Integer x)) (V (Integer y)) = Just $ V (Boolean (x < y))
opLess (V (Float x)) (V (Integer y)) = Just $ V (Boolean (x < fi y))
opLess (V (Integer x)) (V (Float y)) = Just $ V (Boolean (fi x < y))
opLess (V (Char x)) (V (Char y)) = Just $ V (Boolean (x < y))
opLess _ _ = Nothing

opLessEq :: FlatStack -> FlatStack -> Maybe FlatStack
opLessEq (V (Integer x)) (V (Integer y)) = Just $ V (Boolean (x <= y))
opLessEq (V (Float x)) (V (Integer y)) = Just $ V (Boolean (x <= fi y))
opLessEq (V (Integer x)) (V (Float y)) = Just $ V (Boolean (fi x <= y))
opLessEq (V (Char x)) (V (Char y)) = Just $ V (Boolean (x <= y))
opLessEq _ _ = Nothing

opGreater :: FlatStack -> FlatStack -> Maybe FlatStack
opGreater a b = case opLessEq a b of
  Just (V (Boolean result)) -> Just $ V $ Boolean (not result)
  _ -> Nothing

opGreaterEq :: FlatStack -> FlatStack -> Maybe FlatStack
opGreaterEq a b = case opLess a b of
  Just (V (Boolean result)) -> Just $ V $ Boolean (not result)
  _ -> Nothing

opAnd :: FlatStack -> FlatStack -> Maybe FlatStack
opAnd (V (Boolean a)) (V (Boolean b)) = Just $ V $ Boolean (a && b)
opAnd _ _ = Nothing

opOr :: FlatStack -> FlatStack -> Maybe FlatStack
opOr (V (Boolean a)) (V (Boolean b)) = Just $ V $ Boolean (a || b)
opOr _ _ = Nothing

opNot :: FlatStack -> Maybe FlatStack
opNot (V (Boolean a)) = Just $ V $ Boolean (not a)
opNot _ = Nothing

opNeg :: FlatStack -> Maybe FlatStack
opNeg (V (Integer a)) = Just $ V $ Integer (a * (-1))
opNeg (V (Float a)) = Just $ V $ Float (a * (-1))
opNeg _ = Nothing

opList :: Stack -> VarMap -> Int -> Either String Stack
opList s m n = case popN s m n of
  Right (final_stack, tab) -> Right $ Flat (Tab tab):final_stack
  Left e -> Left e

-- BUILTINS
readRef :: [a] -> StackValue -> VarMap -> String -> Either String a
readRef l ind m name = case flatten ind m of
  Right (V (Integer i)) -> if length l >= fi i
                           then Right $ l !! fi i
                           else Left $ "Index out of range for " ++ name
  _ -> Left "Invalid type for index"
  Left e -> Left e

flatten :: StackValue -> VarMap -> Either String FlatStack
flatten (SymVM v) m = case Map.lookup v m of
  Just (Val x) -> flatten (Flat x) m
  _ -> Left $ "undefined variable " ++ v
flatten (Flat x) _ = Right x
flatten (Ref name ind) m = case Map.lookup name m of
  Just (Val (Tab x)) -> readRef x ind m name
  Just (Val (V (Str x))) -> case readRef x ind m name of
    Right new_x -> return (V (Char new_x))
    Left e      -> Left e
  _ -> Left $ "Undefined variable" ++ name

opPrintList :: [FlatStack] -> Bool -> String
opPrintList [] False = "]"
opPrintList list True = "[" ++ opPrintList list False
opPrintList (x:xs) False
  | length (x:xs) == 1 = showFlat x ++ opPrintList xs False
  | otherwise = showFlat x ++ ", " ++ opPrintList xs False

showValue :: Value -> String
showValue (Str s) = s
showValue (Integer i) = show i
showValue (Float f) = show f
showValue (Boolean b) = show b
showValue (Char c) = show c
showValue Nil = "nil"

showFlat :: FlatStack -> String
showFlat (Tab l) = opPrintList l True
showFlat (V l) = showValue l
showFlat (File f) = show f

-- Builtin
type BuiltInFunc = (Stack -> VarMap -> IO (StackValue, Stack))

popN :: Stack -> VarMap -> Int -> Either String (Stack, [FlatStack])
popN s _ 0 = Right (s, [])
popN [] _ _ = Right ([], [])
popN (x:xs) m n = case flatten x m of
  Right v -> case popN xs m (n - 1) of
    Right (final, t) -> Right (final, v:t)
    Left e           -> Left e
  Left e  -> Left e

none :: Monad m => b -> m (StackValue, b)
none f = return (Flat (V Nil), f)

builtInMap :: VarMap
builtInMap = Map.fromList
  [ ("print", BuiltIn printCall)
  , ("typeOf", BuiltIn typeOfCall)
  , ("rand", BuiltIn rand)
  , ("open", BuiltIn open)
  , ("close", BuiltIn closeCall)
  , ("input", BuiltIn inputCall)
  , ("len", BuiltIn lenCall)
    --   , ("write", BuiltIn writeCall)
  , ("read", BuiltIn readCall)]

typeOfVal :: FlatStack -> String
typeOfVal (V (Integer _)) = "integer"
typeOfVal (V (Float _)) = "float"
typeOfVal (V (Char _)) = "char"
typeOfVal (V (Boolean _)) = "boolean"
typeOfVal (V (Str _)) = "string"
typeOfVal (V Nil) = "nil"
typeOfVal (File _) = "file"
typeOfVal (Tab _) = "list"

typeOfCall :: BuiltInFunc
typeOfCall s m = case popN s m 1 of
  Right (final_stack, [v]) -> putStrLn (typeOfVal v) >> none final_stack
  Right _ -> die "typeof takes one argument but none where given"
  Left e -> die e

printCall :: BuiltInFunc
printCall s m = case popN s m 1 of
  Right (f, [x]) -> putStrLn (showFlat x) >> none f
  Right (_, _)   -> die "Print takes one argument but none given"
  Left e         -> die e

rand :: BuiltInFunc
rand s _ = (randomIO :: IO Double) >>= (\x -> return (Flat (V (Float x)), s))

openLogic :: String -> Char -> Stack -> IO (StackValue, Stack)
openLogic filename 'w' s = openFile filename WriteMode
  >>= \f -> return (Flat (File f), s)
openLogic filename 'r' s = openFile filename ReadMode
  >>= \f -> return (Flat (File f), s)
openLogic _ _ _ = die "Open: invalid mode"

open :: BuiltInFunc
open s m = case popN s m 2 of
  Right (f_stack, [V (Str filename), V (Char mode)])
    -> openLogic filename mode f_stack
  Right (_, [V (Str _), _]) -> die "Open: invalid type for mode"
  Right (_, [_, V (Char _)]) -> die "Open: invalid type for mode"
  Right (_, _) -> die "Open: Wrong number of arguments"
  Left e -> die e

readCall :: BuiltInFunc
readCall s m = case popN s m 1 of
  Right (f_stack, [File f]) -> hIsEOF f
    >>= \empty
    -> if empty
       then none f_stack
       else hGetLine f >>= \line -> return (Flat (V (Str line)), f_stack)
  Right (_, _) -> die "Read: not a file"
  Left e -> die e

closeCall :: BuiltInFunc
closeCall s m = case popN s m 1 of
  Right (f_stack, [File f]) -> hClose f >> none f_stack
  Right (_, _) -> die "Close: not a file"
  Left e -> die e

inputCall :: BuiltInFunc
inputCall s _ = hIsEOF stdin
  >>= \empty -> if empty
                then none s
                else getLine >>= \line -> return (Flat (V (Str line)), s)

lenCall :: BuiltInFunc
lenCall s m = case popN s m 1 of
  Right (f_stack, [Tab t]) -> return (f t, f_stack)
  Right (f_stack, [V (Str t)]) -> return (f t, f_stack)
  Right (_, [_]) -> die "Len: invalid type"
  Right _ -> die "len: Wrong number of arguments"
  Left e -> die e
  where
    f l = Flat (V (Integer (fi (length l))))
