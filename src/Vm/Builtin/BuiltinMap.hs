{-
-- EPITECH PROJECT, 2023
-- Test lisp
-- File description:
-- Vm
-}
{-# LANGUAGE DataKinds #-}
module Vm.Builtin.BuiltinMap (builtInMap) where

import qualified Data.Map.Lazy as Map
import           Types
import           System.Random (randomIO, randomRIO)
import           System.Exit (die)
import           Vm.VmTypes
import           Vm.Utils
import           Text.Read (readMaybe)
import           Vm.Builtin.Math (mathMap)
import           Vm.Builtin.Lists (listMap)
import           Vm.Builtin.IO (ioMap)
import System.Clock
    ( getTime, Clock(Realtime), TimeSpec(sec, nsec) )
import System.Process.Extra (callProcess)
import Control.Exception (catch, SomeException)

-- BUILTINS
builtInMap :: VarMap
builtInMap = Map.fromList
  ([ ("typeOf", BuiltIn typeOfCall)
   , ("rand", BuiltIn rand)
   , ("uniform", BuiltIn uniformCall)
   , ("str", BuiltIn stringCall)
   , ("int", BuiltIn intCall)
   , ("float", BuiltIn floatCall)
   , ("randrange", BuiltIn randRangeCall)
   , ("time", BuiltIn timeCall)
   , ("timeit", BuiltIn timeitCall)
   , ("subprocess", BuiltIn subprocess)]
   ++ listMap
   ++ mathMap
   ++ ioMap)

typeOfCall :: BuiltInFunc
typeOfCall s m = case popN s m 1 of
  Right (final_stack, [v]) -> return
    (Flat (V (Str (typeOfVal v))), final_stack)
  Right _ -> die "typeof takes one argument but none where given"
  Left e -> die e

rand :: BuiltInFunc
rand s _ = (randomIO :: IO Double) >>= (\x -> return (Flat (V (Float x)), s))

uniformLogic :: Double -> Double -> Stack -> IO (StackValue, Stack)
uniformLogic a b s = randomRIO (a, b) >>= \x -> return (Flat (V (Float x)), s)

uniformCall :: BuiltInFunc
uniformCall s m = case popN s m 2 of
  Right (s', [V (Float a), V (Float b)]) -> uniformLogic a b s'
  Right (s', [V (Float a), V (Integer b)]) -> uniformLogic a (fi b) s'
  Right (s', [V (Integer a), V (Float b)]) -> uniformLogic (fi a) b s'
  Right (s', [V (Integer a), V (Integer b)]) -> uniformLogic (fi a) (fi b) s'
  Right (_, [_, _]) -> die "uniform: wrong arg type"
  Right (_, _) -> die "uniform: Wrong number of arguments"
  Left e -> die e

randRangeCall :: BuiltInFunc
randRangeCall s m = case popN s m 2 of
  Right (s', [V (Integer a), V (Integer b)]) -> randomRIO (a, b)
    >>= \x -> return (Flat (V (Integer x)), s')
  Right (_, [_, _]) -> die "uniform: wrong arg type"
  Right (_, _) -> die "uniform: Wrong number of arguments"
  Left e -> die e

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

timeitCall :: BuiltInFunc
timeitCall s _ = getTime Realtime >>= (\x -> return (Flat (f x), s))
  where
    f time = Tab [V (Integer (fi (sec time))), V (Integer (fi (nsec time)))]

timeCall :: BuiltInFunc
timeCall s _ = getTime Realtime >>= (\x -> return (Flat (f x), s))
  where
    f time = V (Integer (fi (sec time)))


extractStrings :: [FlatStack] -> Maybe [String]
extractStrings [] = Just []
extractStrings (V (Str x):xs) = case extractStrings xs of
  Nothing  -> Nothing
  Just xs' -> Just $ x:xs'
extractStrings _ = Nothing


execProcess :: String -> [String] -> Stack -> IO (StackValue, Stack)
execProcess command args s = catch (callProcess command args >> return (Flat (V (Integer 0)), s)) handler
  where
    handler ::  SomeException -> IO (StackValue, Stack)
    handler _ = return (Flat (V (Integer 1)), s)

subprocess :: BuiltInFunc
subprocess s m = case popN s m 1 of
  Right (s', [Tab t]) -> case extractStrings t of
    Nothing -> die "subprocess only takes strings"
    Just [] -> die "subprocess no command to execute"
    Just (x:xs) -> execProcess x xs s'
  Right (_, [_]) -> die "subprocess takes string tab"
  Right (_', _) -> die "subprocess takes one argument"
  Left e -> die e
