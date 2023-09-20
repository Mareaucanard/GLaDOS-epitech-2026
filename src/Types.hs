{-# LANGUAGE InstanceSigs #-}
module Types (VarMap, Ast (..), SExpr(..)) where

import qualified Data.Map.Lazy as Map

type VarMap = Map.Map String Ast

data SExpr
  = Integer Int
  | Symbol String
  | List [SExpr]
  | Boolan Bool
  deriving (Show, Read)

data Ast
  = Value Int
  | Sym String
  | Call Ast [Ast]
  | Boolean Bool
  | Lambda ([Ast] -> VarMap -> Either Ast String)
  | None

instance Show Ast where
  show :: Ast -> String
  show (Lambda _) = "Lambda "
  show (Value i) = "Value " ++ show i
  show (Sym s) = "Symbol " ++ show s
  show (Call a b) = "Call " ++ show a ++ " " ++ show b
  show (Boolean b) = "Boolean " ++ show b
  show None = "None"
