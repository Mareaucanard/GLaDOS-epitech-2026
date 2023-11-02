module Vm.VmTypes
    ( Symbol(..)
    , FlatStack(..)
    , StackValue(..)
    , Stack
    , Insts
    , VarMap
    , BuiltInFunc) where

import           Types (Instruction(..), Value(..))
import           System.IO (Handle)
import qualified Data.Map.Lazy as Map

type BuiltInFunc = (Stack -> VarMap -> IO (StackValue, Stack))

data Symbol = Val FlatStack
            | Func [String] [Instruction]
            | BuiltIn (Stack -> VarMap -> IO (StackValue, Stack))

instance Show Symbol where
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
