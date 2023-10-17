module Parsing.VerifyParsing
    ( verifyParsing
    , simplifyParsing
    , extractFunctions
    , verifyFunctionRedefinition) where

import           Types

extractFunctions :: [Ast] -> ([Ast], [Ast])
extractFunctions [] = ([], [])
extractFunctions ((FunctionDefinition name args block):xs) =
  (FunctionDefinition name args block:funcs, not_funcs)
  where
    (funcs, not_funcs) = extractFunctions xs
extractFunctions (x:xs) = (funcs, x:not_funcs)
  where
    (funcs, not_funcs) = extractFunctions xs

simplifyAst :: Ast -> Maybe Ast
simplifyAst (Symbol _) = Nothing
simplifyAst (Const _) = Nothing
simplifyAst (UnaryOp _ _) = Nothing
simplifyAst (BinaryOp Assign x y) = Just (BinaryOp Assign x y)
simplifyAst (BinaryOp {}) = Nothing
simplifyAst (TernaryOp {}) = Nothing
simplifyAst (Block _) = Nothing
simplifyAst None = Nothing
simplifyAst (FunctionDefinition name args b) = Just
  (FunctionDefinition name args (simplifyParsing b))
simplifyAst x = Just x

simplifyParsing :: [Ast] -> [Ast]
simplifyParsing [] = []
simplifyParsing (x:xs) = case simplifyAst x of
  Nothing -> child
  Just y  -> y:child
  where
    child = simplifyParsing xs

verifyInnerAst :: Ast -> Maybe String
verifyInnerAst (BinaryOp Assign (Symbol _) _) = Nothing
verifyInnerAst (BinaryOp Assign (IndexOf _ _) _) = Nothing
verifyInnerAst (BinaryOp Assign _ _) = Just "Tried to assign to non symbol"
verifyInnerAst (FunctionDefinition name _ _) = Just
  $ "Can't define function " ++ name ++ " because it's inside another function"
verifyInnerAst _ = Nothing

verifyInnerParsing :: [Ast] -> Maybe String
verifyInnerParsing [] = Nothing
verifyInnerParsing (x:xs) = case verifyInnerAst x of
  Nothing  -> verifyInnerParsing xs
  Just err -> Just err

verifyRootAst :: Ast -> Maybe String
verifyRootAst (BinaryOp Assign (Symbol _) _) = Nothing
verifyRootAst (BinaryOp Assign (IndexOf _ _) _) = Nothing
verifyRootAst (BinaryOp Assign _ _) = Just "Tried to assign to non symbol"
verifyRootAst (Return _) = Just "Return outside of function"
verifyRootAst (FunctionDefinition _ _ asts) = verifyInnerParsing asts
verifyRootAst _ = Nothing

verifyAstList :: [Ast] -> Maybe String
verifyAstList [] = Nothing
verifyAstList (x:xs) = case verifyRootAst x of
  Just err -> Just err
  Nothing  -> verifyParsing xs

verifyParsing :: [Ast] -> Maybe String
verifyParsing l = case verifyAstList l of
  Nothing  -> verifyFunctionRedefinition l
  Just err -> Just err

redefinitionLogic :: [Ast] -> [String] -> Maybe String
redefinitionLogic [] _ = Nothing
redefinitionLogic ((FunctionDefinition name _ _):xs) l =
  if name `elem` l
  then Just $ "Function " ++ name ++ " is already defined."
  else redefinitionLogic xs (name:l)
redefinitionLogic (_:xs) l = redefinitionLogic xs l

verifyFunctionRedefinition :: [Ast] -> Maybe String
verifyFunctionRedefinition l = redefinitionLogic l []
