module Types
    ( Value(..)
    , UnaryOperator(..)
    , BinaryOperator(..)
    , TernaryOperator(..)
    , Ast(..)
    , Token(..)
    , IfType
    , Instruction(..)) where

import           Data.Int (Int64)

data Value = Integer Int64
           | Float Double
           | Char Char
           | Boolean Bool
           | Str String
           | SymVM String
           | ListVM [Value]
           | Reference (String, Int)
           | Nil
  deriving (Eq, Show)

data UnaryOperator = BoolNot
                   | Negative
  deriving (Eq, Show)

data BinaryOperator =
    Add
  | Sub
  | Times
  | Div
  | Mod
  | Assign
  | Equal
  | Lower
  | LowerEq
  | BoolAnd
  | BoolOr
  | Greater
  | GreaterEq
  | NotEq
  deriving (Eq, Show)

data TernaryOperator = TernaryGate
  deriving (Eq, Show)

data Token =
    Constant Value
  | Sym String
  | OpenParenthesis
  | CloseParenthesis
  | Comma
  | SemiColon
  | OpenBracket
  | CloseBracket
  | OpenBraces
  | CloseBraces
  | Colon
  | Period
  | QuestionMark
  | Blank
  | AddSymbol
  | SubSymbol
  | Unary UnaryOperator
  | Binary BinaryOperator
  | PreProcessor String [Token]
  deriving (Eq, Show)

type IfType = (Ast, [Ast])

data Ast =
    Symbol String
  | Const Value
  | List [Ast]
  | Dict [(Ast, Ast)]
  | UnaryOp UnaryOperator Ast
  | BinaryOp BinaryOperator Ast Ast
  | TernaryOp TernaryOperator Ast Ast Ast
  | Block [Ast]
  | FunctionCall String [Ast]
  | FunctionDefinition String [String] [Ast]
  | IfBlock IfType [IfType] (Maybe [Ast])
  | WhileBlock Ast [Ast]
  | ForBlock Ast Ast Ast [Ast]
  | Return Ast
  | IndexOf String Ast
  | None
  deriving (Eq, Show)

data Instruction =
    Function String [String]
  | Push Value
  | PushSymbol String
  | JIF Int64
  | Jump Int64
  | Print
  | Call
  | Set
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | LET
  | GT
  | GET
  | NEGATIVE
  | TERNARY
  | RET
  | LIST Int64
  | INDEX String
  deriving (Show, Eq)
