module Tokenization (testTokenize) where

import           Test.Hspec
import           Parsing.Tokenize (tokenize)
import           Types (Token(..), BinaryOperator(..), UnaryOperator(..)
                      , Value(..))

tokenIt :: String -> String -> Either [Token] String -> SpecWith ()
tokenIt desc str res = it desc $ tokenize str `shouldBe` res

testTokenize :: IO ()
testTokenize = hspec
  $ do
    describe "all symbols"
      $ do
        tokenIt ";" ";" $ Left [SemiColon]
        tokenIt "(" "(" $ Left [OpenParenthesis]
        tokenIt ")" ")" $ Left [CloseParenthesis]
        tokenIt "," "," $ Left [Comma]
        tokenIt "." "." $ Left [Period]
        tokenIt "[" "[" $ Left [OpenBracket]
        tokenIt "]" "]" $ Left [CloseBracket]
        tokenIt "{" "{" $ Left [OpenBraces]
        tokenIt "}" "}" $ Left [CloseBraces]
        tokenIt ":" ":" $ Left [Colon]
        tokenIt "?" "?" $ Left [QuestionMark]
        tokenIt "+" "+" $ Left [AddSymbol]
        tokenIt "-" "-" $ Left [SubSymbol]
        tokenIt "*" "*" $ Left [Binary Times]
        tokenIt "/" "/" $ Left [Binary Div]
        tokenIt "%" "%" $ Left [Binary Mod]
        tokenIt "and " "and " $ Left [Binary BoolAnd]
        tokenIt "or " "or " $ Left [Binary BoolOr]
        tokenIt "==" "==" $ Left [Binary Equal]
        tokenIt "=" "=" $ Left [Binary Assign]
        tokenIt "<=" "<=" $ Left [Binary LowerEq]
        tokenIt "<" "<" $ Left [Binary Lower]
        tokenIt ">=" ">=" $ Left [Binary GreaterEq]
        tokenIt ">" ">" $ Left [Binary Greater]
        tokenIt "!=" "!=" $ Left [Binary NotEq]
        tokenIt "not " "not " $ Left [Unary BoolNot]
    describe "constants"
      $ do
        tokenIt "int positive" "4" $ Left [Constant (Integer 4)]
        tokenIt "int negative" "-4" $ Left [Constant (Integer (-4))]
        tokenIt "float positive" "3.14" $ Left [Constant (Float 3.14)]
        tokenIt "float negative" "-3.14" $ Left [Constant (Float (-3.14))]
        tokenIt "True" "True" $ Left [Constant (Boolean True)]
        tokenIt "False" "False" $ Left [Constant (Boolean False)]
        tokenIt "nil" "nil" $ Left [Constant Nil]
        tokenIt "char" "'h'" $ Left [Constant (Char 'h')]
        tokenIt "string" "\"hello\"" $ Left [Constant (Str "hello")]
        tokenIt "empty string" "\"\"" $ Left [Constant (Str "")]
    tokenIt "symbol" "x" $ Left [Sym "x"]
    tokenIt "symbol_complex" "x_3-2" $ Left [Sym "x_3-2"]
    tokenIt "blank" " " $ Left []
    tokenIt "preprocessing" "#define pi (3.14)" $ Left [PreProcessor "pi" [Constant (Float 3.14)]]
    tokenIt "preprocessing no space" "#define pi(3.14)" $ Left [PreProcessor "pi" [Constant (Float 3.14)]]
