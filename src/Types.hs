{-# LANGUAGE InstanceSigs #-}
module Types (VarMap, Ast (..), SExpr(..)) where

import qualified Data.Map.Lazy as Map

type VarMap = Map.Map String Ast

data SExpr
  = Integer Int -- ^ An integer
  | Symbol String -- ^ A symbol
  | List [SExpr] -- ^ A list of SExpr
  | Boolan Bool -- ^ A boolean
  deriving (Show -- ^ Makes SExpr printable
    , Read -- ^ Makes SExpr readable
  )
data Ast
  = Value Int -- ^ An integer
  | Sym String -- ^ A symbol
  | Call Ast [Ast] -- ^ A call
  | Boolean Bool -- ^ A boolean
  | Lambda ([Ast] -> VarMap -> Either Ast String) -- ^ A lambda
  | None -- ^ None

-- |Makes Ast printable.
instance Show Ast where
  show :: Ast -> String -- ^ The return value
  show (Lambda _) = "Lambda"
  show (Value i) = "Value " ++ show i
  show (Sym s) = "Symbol " ++ show s
  show (Call a b) = "Call " ++ show a ++ " " ++ show b
  show (Boolean b) = "Boolean " ++ show b
  show None = "None"
