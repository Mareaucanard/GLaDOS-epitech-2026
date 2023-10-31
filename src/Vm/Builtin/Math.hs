module Vm.Builtin.Math (mathMap) where

import Vm.VmTypes
import Types
import System.Exit (die)
import Vm.Utils (popN)

mathMap :: [(String, Symbol)]
mathMap = [ ("sqrt", BuiltIn (builtinNum sqrt "sqrt"))
          , ("cos", BuiltIn (builtinNum cos "cos"))
          , ("acos", BuiltIn (builtinNum acos "acos"))
          , ("sin", BuiltIn (builtinNum sin "sin"))
          , ("asin", BuiltIn (builtinNum asin "asin"))
          , ("tan", BuiltIn (builtinNum tan "tan"))
          , ("atan", BuiltIn (builtinNum atan "atan"))
          , ("log", BuiltIn (builtinNum log "log"))
          , ("exp", BuiltIn (builtinNum exp "exp"))]

builtinNum :: (Double -> Double) -> String -> BuiltInFunc
builtinNum f name s m = case popN s m 1 of
  Right (s', [V (Integer t)]) -> return
    (Flat (V (Float (sqrt (fromIntegral t)))), s')
  Right (s', [V (Float t)]) -> return (Flat (V (Float (f t))), s')
  Right (_, [_]) -> die $ name ++ ": invalid type"
  Right _ -> die $ name ++ ": Wrong number of arguments"
  Left e -> die e
