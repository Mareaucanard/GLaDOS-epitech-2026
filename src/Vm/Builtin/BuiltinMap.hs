{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isEOF" #-}
module Vm.Builtin.BuiltinMap (builtInMap) where

import qualified Data.Map.Lazy as Map
import           Types
import           System.Random (randomIO)
import           System.Exit (die)
import           Vm.VmTypes
import           Vm.Utils
import           Text.Read (readMaybe)
import Vm.Builtin.Math (mathMap)
import Vm.Builtin.Lists (listMap)
import Vm.Builtin.IO (ioMap)

-- BUILTINS

builtInMap :: VarMap
builtInMap = Map.fromList
  ([
   ("typeOf", BuiltIn typeOfCall)
  , ("rand", BuiltIn rand)
  , ("str", BuiltIn stringCall)
  , ("int", BuiltIn intCall)
  , ("float", BuiltIn floatCall)
  ] ++ listMap ++ mathMap ++ ioMap)

typeOfCall :: BuiltInFunc
typeOfCall s m = case popN s m 1 of
  Right (final_stack, [v]) -> return
    (Flat (V (Str (typeOfVal v))), final_stack)
  Right _ -> die "typeof takes one argument but none where given"
  Left e -> die e

rand :: BuiltInFunc
rand s _ = (randomIO :: IO Double) >>= (\x -> return (Flat (V (Float x)), s))

stringCall :: BuiltInFunc
stringCall s m = case popN s m 1 of
  Right (f_stack, [x]) -> return (f (showFlat x), f_stack)
  Right _ -> die "str: Wrong number of arguments"
  Left e -> die e
  where
    f l = Flat (V (Str l))

intCall :: BuiltInFunc
intCall s m = case popN s m 1 of
  Right (f_stack, [V (Str str)]) -> case (readMaybe str :: Maybe Int) of
    Nothing -> die $ "int: can't read " ++ show str
    Just x  -> return (f fromIntegral x, f_stack)
  Right (f_stack, [V (Float x)]) -> return (f floor x, f_stack)
  Right (f_stack, [V (Integer x)]) -> return (f id x, f_stack)
  Right _ -> die "int: Wrong number of arguments"
  Left e -> die e
  where
    f f' x = Flat (V (Integer (f' x)))

floatCall :: BuiltInFunc
floatCall s m = case popN s m 1 of
  Right (f_stack, [V (Str str)]) -> case (readMaybe str :: Maybe Double) of
    Nothing -> die $ "float: can't read " ++ show str
    Just x  -> return (f id x, f_stack)
  Right (f_stack, [V (Float x)]) -> return (f id x, f_stack)
  Right (f_stack, [V (Integer x)]) -> return (f fromIntegral x, f_stack)
  Right _ -> die "float: Wrong number of arguments"
  Left e -> die e
  where
    f f' x = Flat (V (Float (f' x)))
