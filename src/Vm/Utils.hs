module Vm.Utils (popN, none, flatten, typeOfVal, fi, showFlat, showValue) where

import           Types
import           Vm.VmTypes (VarMap, Stack, StackValue(..), FlatStack(..)
                           , Symbol(..))
import qualified Data.Map.Lazy as Map

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

typeOfVal :: FlatStack -> String
typeOfVal (V (Integer _)) = "integer"
typeOfVal (V (Float _)) = "float"
typeOfVal (V (Char _)) = "char"
typeOfVal (V (Boolean _)) = "boolean"
typeOfVal (V (Str _)) = "string"
typeOfVal (V Nil) = "nil"
typeOfVal (File _) = "file"
typeOfVal (Tab _) = "list"

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

readRef :: [a] -> StackValue -> VarMap -> String -> Either String a
readRef l ind m name = case flatten ind m of
  Right (V (Integer i)) -> if length l > fi i
                           then Right $ l !! fi i
                           else Left $ "Index out of range for " ++ name
  Left e -> Left e
  _ -> Left "Invalid type for index"

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

opPrintList :: [FlatStack] -> Bool -> String
opPrintList [] False = "]"
opPrintList list True = "[" ++ opPrintList list False
opPrintList (x:xs) False
  | length (x:xs) == 1 = showFlat x ++ opPrintList xs False
  | otherwise = showFlat x ++ ", " ++ opPrintList xs False
