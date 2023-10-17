module TokenParsing (testParsing) where

import           Test.Hspec
import           Types (Token(..), BinaryOperator(..), Value(..), Ast(..))
import           Parsing.TokenParser (tokensToAst)

parseIt :: String -> [Token] -> Either [Ast] String -> SpecWith ()
parseIt desc tokens res = it desc $ tokensToAst tokens `shouldBe` res

parseInfix :: String -> Token -> BinaryOperator -> SpecWith ()
parseInfix desc token res = parseIt
  desc
  [Constant (Integer 0), token, Constant (Integer 1), SemiColon]
  (Left [BinaryOp res (Const (Integer 0)) (Const (Integer 1))])

testParsing :: IO ()
testParsing = hspec
  $ do
    describe "infix operators"
      $ do
        parseInfix "+" AddSymbol Add
        parseInfix "-" SubSymbol Sub
        parseInfix "*" (Binary Times) Times
        parseInfix "/" (Binary Div) Div
        parseInfix "%" (Binary Mod) Mod
        parseInfix "=" (Binary Assign) Assign
        parseInfix "==" (Binary Equal) Equal
        parseInfix "<" (Binary Lower) Lower
        parseInfix "<=" (Binary LowerEq) LowerEq
        parseInfix "and" (Binary BoolAnd) BoolAnd
        parseInfix "or" (Binary BoolOr) BoolOr
        parseInfix ">" (Binary Greater) Greater
        parseInfix ">=" (Binary GreaterEq) GreaterEq
        parseInfix "!=" (Binary NotEq) NotEq
    describe "raw values"
      $ do
        parseIt "int" [Constant (Integer 1), SemiColon]
          $ Left [Const (Integer 1)]
        parseIt "float" [Constant (Float 3.14), SemiColon]
          $ Left [Const (Float 3.14)]
        parseIt "char" [Constant (Char 'h'), SemiColon]
          $ Left [Const (Char 'h')]
        parseIt "string" [Constant (Str "hello"), SemiColon]
          $ Left [Const (Str "hello")]
        parseIt "True" [Constant (Boolean True), SemiColon]
          $ Left [Const (Boolean True)]
        parseIt "False" [Constant (Boolean False), SemiColon]
          $ Left [Const (Boolean False)]
        parseIt "nil" [Constant Nil, SemiColon] $ Left [Const Nil]
    describe "bad tokens"
      $ do
        it "no semicolon"
          $ tokensToAst [SemiColon, Sym "a"]
          `shouldBe` Right "failed to parse file"
    parseIt "symbol" [Sym "a", SemiColon] $ Left [Symbol "a"]
    parseIt
      "function"
      [ Sym "function"
      , Sym "double"
      , OpenParenthesis
      , Sym "x"
      , CloseParenthesis
      , OpenBraces
      , Sym "return"
      , Sym "x"
      , Binary Times
      , Constant (Integer 2)
      , SemiColon
      , CloseBraces]
      $ Left
        [ FunctionDefinition
            "double"
            ["x"]
            [Return (BinaryOp Times (Symbol "x") (Const (Integer 2)))]]
    parseIt
      "if"
      [ Sym "if"
      , OpenParenthesis
      , Constant (Boolean False)
      , CloseParenthesis
      , OpenBraces
      , CloseBraces
      , Sym "elif"
      , OpenParenthesis
      , Constant (Boolean True)
      , CloseParenthesis
      , OpenBraces
      , CloseBraces
      , Sym "else"
      , OpenBraces
      , CloseBraces]
      $ Left
        [ IfBlock
            (Const (Boolean False), [])
            [(Const (Boolean True), [])]
            (Just [])]
    parseIt
      "for"
      [ Sym "for"
      , OpenParenthesis
      , Sym "x"
      , Binary Assign
      , Constant (Integer 0)
      , SemiColon
      , Sym "x"
      , Binary LowerEq
      , Constant (Integer 100)
      , SemiColon
      , Sym "x"
      , Binary Assign
      , Sym "x"
      , AddSymbol
      , Constant (Integer 1)
      , CloseParenthesis
      , OpenBraces
      , CloseBraces]
      $ Left
        [ ForBlock
            (BinaryOp Assign (Symbol "x") (Const (Integer 0)))
            (BinaryOp LowerEq (Symbol "x") (Const (Integer 100)))
            (BinaryOp
               Assign
               (Symbol "x")
               (BinaryOp Add (Symbol "x") (Const (Integer 1))))
            []]
    parseIt
      "while"
      [ Sym "while"
      , OpenParenthesis
      , Sym "x"
      , Binary Lower
      , Constant (Integer 100)
      , CloseParenthesis
      , OpenBraces
      , Sym "x"
      , Binary Assign
      , Sym "x"
      , AddSymbol
      , Constant (Integer 1)
      , SemiColon
      , CloseBraces]
      $ Left
        [ WhileBlock
            (BinaryOp Lower (Symbol "x") (Const (Integer 100)))
            [ BinaryOp
                Assign
                (Symbol "x")
                (BinaryOp Add (Symbol "x") (Const (Integer 1)))]]
