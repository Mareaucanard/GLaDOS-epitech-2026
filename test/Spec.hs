import Test.Hspec

import qualified Data.Map.Lazy as Map
import Ast (sexprToAST, evalAst, mapEvalCalls)
import Lib (printTree, parseString)
import Types (Ast (..), SExpr (..))
import DefaultSymbol (defaultSymbols)

evalDefault :: Ast -> Either Ast String
evalDefault ast = case evalAst ast defaultSymbols of
  Right err -> Right err
  Left (x, _) -> Left x

main :: IO ()
main = hspec $ do
  describe "sexprToAST" $ do
    it "converts Boolean SExpr to Boolean Ast" $ do
      let sexpr = Boolan True
      sexprToAST sexpr `shouldBe` Left (Boolean True)

    it "converts Symbol SExpr to Sym Ast" $ do
      let sexpr = Symbol "x"
      sexprToAST sexpr `shouldBe` Left (Sym "x")

    it "handles empty list" $ do
      let sexpr = List []
      sexprToAST sexpr `shouldBe` Left None

    it "handles invalid input" $ do
      let sexpr = List [Symbol "lambda"]
      sexprToAST sexpr `shouldBe` Right "Invalid number of arguments for lambda"

  describe "evalAst" $ do
    it "evaluates Value Ast and preserves variable map" $ do
      let ast = Value 42
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Left (Value 42, varMap)

    it "evaluates Symbol Ast and retrieves corresponding variable value" $ do
      let ast = Sym "x"
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Left (Value 10, varMap)

    it "evaluates Call Ast and performs operation" $ do
      let ast = Call (Sym "+") [Value 5, Value 3]
      evalDefault ast  `shouldBe` Left (Value 8)

    it "evaluates Lambda Ast and preserves variable map" $ do
      let ast = Lambda (\_ _ -> Left None)
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Left (Lambda (\_ _ -> Left None), varMap)

    it "evaluates None Ast and preserves variable map" $ do
      let ast = None
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Left (None, varMap)

    it "evaluates Boolean Ast and preserves variable map" $ do
      let ast = Boolean True
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Left (Boolean True, varMap)

    it "returns error message for undefined Symbol Ast" $ do
      let ast = Sym "z"
      let varMap = Map.fromList [("x", Value 10), ("y", Value 20)]
      evalAst ast varMap `shouldBe` Right "Unknown symbol z"

  describe "Default symbols" $ do
    describe "Arithmetic" $ do
      let applySymbol str = Call (Sym str) [Value 1, Value 2]
      it "+ operator" $ do
        let ast = applySymbol "+"
        evalDefault ast `shouldBe` Left (Value 3)
      it "- operator" $ do
        let ast = applySymbol "-"
        evalDefault ast `shouldBe` Left (Value (-1))
      it "* operator" $ do
        let ast = applySymbol "*"
        evalDefault ast `shouldBe` Left (Value 2)
      it "div operator" $ do
        let ast = applySymbol "div"
        evalDefault ast `shouldBe` Left (Value 0)
      it "mod operator" $ do
        let ast = applySymbol "mod"
        evalDefault ast `shouldBe` Left (Value 1)
      it "** operator" $ do
        let ast = applySymbol "**"
        evalDefault ast `shouldBe` Left (Value 1)

      it "div 0" $ evalDefault (Call (Sym "div") [Value 1, Value 0]) `shouldBe` Right "Division by zero"
      it "mod 0" $ evalDefault (Call (Sym "mod") [Value 1, Value 0]) `shouldBe` Right "Mod by zero"

      it "+ 0 args" $ evalDefault (Call (Sym "+") []) `shouldBe` Right "Invalid use of add operation"
      it "- 0 args" $ evalDefault (Call (Sym "-") []) `shouldBe` Right "Invalid use of sub operation"
      it "* 0 args" $ evalDefault (Call (Sym "*") []) `shouldBe` Right "Invalid use of times operation"
      it "div 0 args" $ evalDefault (Call (Sym "div") []) `shouldBe` Right "Invalid use of div operation"
      it "mod 0 args" $ evalDefault (Call (Sym "mod") []) `shouldBe` Right "Invalid use of mod operation"
      it "** 0 args" $ evalDefault (Call (Sym "**") []) `shouldBe` Right "Invalid use of power operation"
    describe "Logic" $ do
      it "if function true" $ do
        let ast = Call (Sym "if") [Boolean True, Value 4, Value 5]
        evalDefault ast `shouldBe` Left (Value 4)
      it "if function false" $ do
        let ast = Call (Sym "if") [Boolean False, Value 4, Value 5]
        evalDefault ast `shouldBe` Left (Value 5)

      it "eq? function true" $ evalDefault (Call (Sym "eq?") [Value 1, Value 1]) `shouldBe` Left (Boolean True)
      it "eq? function false" $ evalDefault (Call (Sym "eq?") [Value 1, Value 2]) `shouldBe` Left (Boolean False)
      it "== function true" $ evalDefault (Call (Sym "==") [Value 1, Value 1]) `shouldBe` Left (Boolean True)
      it "== function false" $ evalDefault (Call (Sym "==") [Value 1, Value 2]) `shouldBe` Left (Boolean False)

      it "!= function true" $ evalDefault (Call (Sym "!=") [Value 1, Value 2]) `shouldBe` Left (Boolean True)
      it "!= function false" $ evalDefault (Call (Sym "!=") [Value 1, Value 1]) `shouldBe` Left (Boolean False)
      it "/= function true" $ evalDefault (Call (Sym "/=") [Value 1, Value 2]) `shouldBe` Left (Boolean True)
      it "/= function false" $ evalDefault (Call (Sym "/=") [Value 1, Value 1]) `shouldBe` Left (Boolean False)
      it "=/ function true" $ evalDefault (Call (Sym "=/") [Value 1, Value 2]) `shouldBe` Left (Boolean True)
      it "=/ function false" $ evalDefault (Call (Sym "=/") [Value 1, Value 1]) `shouldBe` Left (Boolean False)

      it "< function true" $ evalDefault (Call (Sym "<") [Value 1, Value 2]) `shouldBe` Left (Boolean True)
      it "< function false" $ evalDefault (Call (Sym "<") [Value 1, Value 0]) `shouldBe` Left (Boolean False)
      it "<= function true" $ evalDefault (Call (Sym "<=") [Value 1, Value 2]) `shouldBe` Left (Boolean True)
      it "<= function false" $ evalDefault (Call (Sym "<=") [Value 1, Value 0]) `shouldBe` Left (Boolean False)
      it "> function true" $ evalDefault (Call (Sym ">") [Value 1, Value 2]) `shouldBe` Left (Boolean False)
      it "> function false" $ evalDefault (Call (Sym ">") [Value 1, Value 0]) `shouldBe` Left (Boolean True)
      it ">= function true" $ evalDefault (Call (Sym ">=") [Value 1, Value 2]) `shouldBe` Left (Boolean False)
      it ">= function false" $ evalDefault (Call (Sym ">=") [Value 1, Value 0]) `shouldBe` Left (Boolean True)
    describe "Lists" $ do
      it "empty list" $ evalDefault (Call (Sym "list") []) `shouldBe` Left (Tab [])
      it "1 elem list" $ evalDefault (Call (Sym "list") [Value 1]) `shouldBe` Left (Tab [Value 1])
      it "2 elem list" $ evalDefault (Call (Sym "list") [Value 1, Tab []]) `shouldBe` Left (Tab [Value 1, Tab []])

      it "car" $ evalDefault (Call (Sym "car") [Tab [Value 1, Value 2, Value 3]]) `shouldBe` Left (Value 1)
      it "cdr" $ evalDefault (Call (Sym "cdr") [Tab [Value 1, Value 2, Value 3]]) `shouldBe` Left (Tab [Value 2, Value 3])
      it "normal cons" $ evalDefault (Call (Sym "cons") [Value 1, Tab [Value 2, Value 3]]) `shouldBe` Left (Tab [Value 1, Value 2, Value 3])
      it "non list tail cons" $ evalDefault (Call (Sym "cons") [Value 1, Value 2]) `shouldBe` Left (Tab [Value 1, Value 2])

      it "isEmpty true" $ evalDefault (Call (Sym "isempty") [Tab []]) `shouldBe` Left (Boolean True)
      it "isEmpty false" $ evalDefault (Call (Sym "isempty") [Tab [Boolean True]]) `shouldBe` Left (Boolean False)

    describe "boolean operator" $ do
      it "and true" $ evalDefault (Call (Sym "and") [Boolean True, Boolean True]) `shouldBe` Left (Boolean True)
      it "and true" $ evalDefault (Call (Sym "and") [Boolean True, Boolean False]) `shouldBe` Left (Boolean False)
      it "or true" $ evalDefault (Call (Sym "or") [Boolean True, Boolean False]) `shouldBe` Left (Boolean True)
      it "or true" $ evalDefault (Call (Sym "or") [Boolean False, Boolean False]) `shouldBe` Left (Boolean False)
      it "not true" $ evalDefault (Call (Sym "not") [Boolean True]) `shouldBe` Left (Boolean False)
      it "not true" $ evalDefault (Call (Sym "not") [Boolean False]) `shouldBe` Left (Boolean True)

    describe "binary arithmetic" $ do
      let applySymbol str = Call (Sym str) [Value 12, Value 27]
      it "& operator" $ do
        let ast = applySymbol "&"
        evalDefault ast `shouldBe` Left (Value 8)
      it "| operator" $ do
        let ast = applySymbol "|"
        evalDefault ast `shouldBe` Left (Value 31)
      it "^ operator" $ do
        let ast = applySymbol "^"
        evalDefault ast `shouldBe` Left (Value 23)
      it "~ operator" $ do
        let ast =  Call (Sym "~") [Value 12]
        evalDefault ast `shouldBe` Left (Value (-13))
      it "<< operator" $ do
        let ast = applySymbol "<<"
        evalDefault ast `shouldBe` Left (Value 1610612736)
      it ">> operator" $ do
        let ast = applySymbol ">>"
        evalDefault ast `shouldBe` Left (Value 0)

    it "nil" $ do evalDefault (Sym "nil") `shouldBe` Left None
    it "empty" $ do evalDefault (Sym "empty") `shouldBe` Left (Tab [])

  describe "keywords" $ do
    it "define" $ do
      let expected_result = Map.insert "x" (Value 3) defaultSymbols
      let ast = Call (Sym "define") [Sym "x", Value 3]
      evalAst ast defaultSymbols `shouldBe` Left (None, expected_result)

    it "define named function" $ do
      let expected_result = Map.insert "x" (Lambda undefined) defaultSymbols
      let ast = Call (Sym "define") [Call (Sym "x") [Sym "y"], Value 3]
      evalAst ast defaultSymbols `shouldBe` Left (None, expected_result)

  describe "show ast" $ do
    it "lambda" $ show (Lambda undefined) `shouldBe` "#<procedure>"
    it "tab" $ show (Tab [Value 1, Value 2, Value 3]) `shouldBe` "[1,2,3]"
    it "value" $ show (Value 5) `shouldBe` "5"
    it "symbol" $ show (Sym "hello_world") `shouldBe` "Symbol \"hello_world\""
    it "bool true" $ show (Boolean True) `shouldBe` "#t"
    it "bool false" $ show (Boolean False) `shouldBe` "#f"
    it "None" $ show None `shouldBe` "None"
    it "Call" $ show (Call (Sym "+") [Value 1, Value 1]) `shouldBe` "Call Symbol \"+\" [1,1]"
    it "Eq1" $ (Call (Sym "h") [Value 1, Value 2]) == (Call (Sym "w") [Value 1, Value 2]) `shouldBe` False
    it "Eq2" $ (Call (Sym "h") [Value 1, Value 2]) == (Call (Sym "h") [Value 1, Value 3]) `shouldBe` False
    it "Eq3" $ (Call (Sym "h") [Value 1, Value 2]) == (Call (Sym "h") [Value 1, Value 2]) `shouldBe` True

  describe "mapEvalCalls" $ do
    it "evaluates empty list of arguments" $ do
      let args = []
      let varMap = Map.empty
      mapEvalCalls args varMap `shouldBe` Left ([], varMap)

    it "evaluates list of arguments with single valid expression" $ do
      let args = [Value 10, Value 20, Value 30]
      let varMap = Map.empty
      mapEvalCalls args varMap `shouldBe` Left (args, varMap)

    it "evaluates list of arguments with valid and invalid expressions" $ do
      let validArgs = [Value 10, Value 20]
      let invalidArg = Sym "x"
      let args = validArgs ++ [invalidArg]
      let varMap = Map.empty
      mapEvalCalls args varMap `shouldBe` Right "Unknown symbol x"

  describe "printTree" $ do
    it "prints simple SExpr" $ do
      let sexpr = Symbol "x"
      printTree sexpr `shouldBe` "A symbol \"x\""

    it "print complete SExpr" $ do
      let sexpr = List [Symbol "define", Symbol "y", Integer 5]
      printTree sexpr `shouldBe` "A list with a symbol \"define\", a symbol \"y\" and an integer 5"

    it "prints nested SExpr" $ do
      let sexpr = List [Symbol "define", Symbol "y", List [Symbol "+", Integer 5, Symbol "x"]]
      printTree sexpr `shouldBe` "A list with a symbol \"define\", a symbol \"y\" and (a list with a symbol \"+\", an integer 5 and a symbol \"x\")"

  describe "parseString" $ do
    it "parse empty string" $ do
      let str = []
      case parseString str of
        Left _ -> fail "Expected Right, but got Left"
        Right expr -> expr `shouldBe` "Can't evaluate empty string"

    describe "simple parsing" $ do
      it "parse num value" $ parseString "3" `shouldBe` Left (Integer 3)
      it "parse symbol value" $ parseString "x" `shouldBe` Left (Symbol "x")
      it "parse empty list" $ parseString "()" `shouldBe` Left (List [])
      it "parse true" $ parseString "#t" `shouldBe` Left (Boolan True)
      it "parse false" $ parseString "#f" `shouldBe` Left (Boolan False)
      it "parse simple call" $ parseString "(+ 1 2)" `shouldBe` Left (List [Symbol "+", Integer 1, Integer 2])
      it "parse with newline" $ parseString "(\n)" `shouldBe` Left (List [])
      it "parse with newline separation" $ parseString "(s1\ns2)" `shouldBe` Left (List [Symbol "s1", Symbol "s2"])

    describe "advanced parsing" $ do
      let argNames = List [Symbol "a", Symbol "b"]
      let op = List [Symbol "+", Symbol "x", Symbol "y"]
      let lambda = List [Symbol "lambda", argNames, op]
      let expected_result = List [lambda, Integer 1, Integer 2]
      it "lambda call" $ parseString "((lambda (a b) (+ x y)) 1 2)" `shouldBe` Left expected_result

      it "call custom lambda" $ do
        let newOp = List [Symbol "+", Symbol "a", Symbol "b"]
        let newLambda = List [Symbol "lambda", argNames, newOp]
        let sexpr = List [newLambda, Integer 1, Integer 2]
        case sexprToAST sexpr of
          Right r-> fail $ "Expected Left, but got Right " ++ show r
          Left preEvalAst -> evalDefault preEvalAst `shouldBe` Left (Value 3)

