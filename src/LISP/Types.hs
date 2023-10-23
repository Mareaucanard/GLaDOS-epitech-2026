-- |
-- = EPITECH PROJECT, 2023
-- glados
--
-- = File description:
-- Types


{-# LANGUAGE InstanceSigs #-}
module LISP.Types (VarMap, Ast (..), SExpr(..)) where

import qualified Data.Map.Lazy as Map

-- | A map of variables.
type VarMap
  = Map.Map String Ast -- ^ The map of variables

-- | An SExpr.
data SExpr
  = Integer Int -- ^ An integer
  | Symbol String -- ^ A symbol
  | List [SExpr] -- ^ A list of SExpr
  | Boolan Bool -- ^ A boolean
  deriving (Show, Eq -- ^ For unit tests
  )

-- | An Ast.
data Ast
  = Value Int -- ^ An integer
  | Sym String -- ^ A symbol
  | Call Ast [Ast] -- ^ A call
  | Boolean Bool -- ^ A boolean
  | Lambda ([Ast] -> VarMap -> Either Ast String) -- ^ A lambda
  | Tab [Ast] -- ^ A list of Ast
  | None -- ^ None

-- Manually implement Eq for Ast
instance Eq Ast where
  (Value x) == (Value y) = x == y
  (Sym x) == (Sym y) = x == y
  (Call a1 args1) == (Call a2 args2) = a1 == a2 && args1 == args2
  (Boolean x) == (Boolean y) = x == y
  -- Decide how to compare Lambda values here (e.g., consider them equal if they have the same function signature)
  (Lambda _) == (Lambda _) = True
  (Tab x) == (Tab y) = x == y
  None == None = True
  _ == _ = False

-- |Makes Ast printable.
instance Show Ast where
  show :: Ast -> String -- ^ The return value
  show (Lambda _) = "#<procedure>"
  show (Tab t) = show t
  show (Value i) = show i
  show (Sym s) = "Symbol " ++ show s
  show (Call a b) = "Call " ++ show a ++ " " ++ show b
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show None = "None"
