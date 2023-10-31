module Vm.Instructions (exec, call) where

import           Types
import           Vm.VmTypes
import qualified Data.Map.Lazy as Map
import           System.Exit (die)
import           Vm.Utils

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
exec (RET:_) (x:xs) m _ = case flatten x m of
  Left e   -> die e
  Right x' -> return (Flat x', xs)
exec (RET:_) [] _ _ = die "STACK ERROR INVALID RETURN"
exec [] s _ _ = return (Flat (V Nil), s)
exec ((Function _ _):_) _ _ _ = die "STACK ERROR FUNCTION IN EXEC"

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
set :: Stack -> VarMap -> Insts -> Insts -> IO (VarMap, Stack)
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
  (Ref name val, tmp_stack) -> case Map.lookup name m of
    Nothing -> die $ "Undefined variable" ++ name
    Just (Val (Tab l)) -> case flatten val m of
      Left e -> die e
      Right (V (Integer flat_i))
        -> if length l > fi flat_i
           then case popN tmp_stack m 1 of
             Right (f_stack, [x]) -> return
               (Map.insert name (Val (Tab l')) m, f_stack)
               where
                 l' = setAt l (fi flat_i) x
             Right _ -> die "STACK ERROR"
             Left e -> die e
           else die $ "Index out of range for " ++ name
      Right _ -> die $ "Can't subscript " ++ name ++ " not an index"
    Just _ -> die $ name ++ " is not a list"

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

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
opAdd (V (Float x)) (V (Float y)) = Just $ V (Float (x + y))
opAdd (V (Str x)) (V (Str y)) = Just $ V (Str (x ++ y))
opAdd _ _ = Nothing

opSub :: FlatStack -> FlatStack -> Maybe FlatStack
opSub (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x - y))
opSub (V (Float x)) (V (Integer y)) = Just $ V (Float (x - fi y))
opSub (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x - y))
opSub (V (Float x)) (V (Float y)) = Just $ V (Float (x - y))
opSub _ _ = Nothing

opMul :: FlatStack -> FlatStack -> Maybe FlatStack
opMul (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x * y))
opMul (V (Float x)) (V (Integer y)) = Just $ V (Float (x * fi y))
opMul (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x * y))
opMul (V (Float x)) (V (Float y)) = Just $ V (Float (x * y))

opMul _ _ = Nothing

opDiv :: FlatStack -> FlatStack -> Maybe FlatStack
opDiv (V (Integer x)) (V (Integer y)) = Just $ V (Integer (x `div` y))
opDiv (V (Float x)) (V (Integer y)) = Just $ V (Float (x / fi y))
opDiv (V (Integer x)) (V (Float y)) = Just $ V (Float (fi x / y))
opDiv (V (Float x)) (V (Float y)) = Just $ V (Float (x / y))
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
opLess (V (Float x)) (V (Float y)) = Just $ V (Boolean (x < y))
opLess (V (Char x)) (V (Char y)) = Just $ V (Boolean (x < y))
opLess _ _ = Nothing

opLessEq :: FlatStack -> FlatStack -> Maybe FlatStack
opLessEq (V (Integer x)) (V (Integer y)) = Just $ V (Boolean (x <= y))
opLessEq (V (Float x)) (V (Integer y)) = Just $ V (Boolean (x <= fi y))
opLessEq (V (Integer x)) (V (Float y)) = Just $ V (Boolean (fi x <= y))
opLessEq (V (Float x)) (V (Float y)) = Just $ V (Boolean (x <= y))
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
