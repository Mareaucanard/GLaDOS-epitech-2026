import Test.Hspec

import qualified Data.Map.Lazy as Map
import Ast (sexprToAST, evalAst, mapEvalCalls, createLambda)
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

    describe "Errors" $ do
      describe "Lambda args" $ do
        let lambda = createLambda ["a", "b"] (Call (Sym "+") [Sym "a", Sym "b"])
        it "Not enough" $ evalDefault (Call lambda []) `shouldBe` Right "Not enough arguments"
        it "Too many" $ evalDefault (Call lambda [Tab [], Tab [], Tab [], Tab []]) `shouldBe` Right "Too many arguments"
        it "Can't evaluate" $ do
          let badCall = Sym "undefined"
          evalDefault (Call lambda [badCall, Value 2]) `shouldBe` evalDefault badCall
      describe "Lambda call" $ do
        let lambda = createLambda [] (Sym "undefined")
        it "Bad lambda expression" $ evalDefault (Call lambda []) `shouldBe` evalDefault (Sym "undefined")
        let lambda2 = createLambda ["a", "b"] (Call (Sym "+") [Sym "a", Sym "b"])
        let args = [Tab [], Value 1]
        it "Normal error in expression" $ evalDefault (Call lambda2 args) `shouldBe` evalDefault (Call (Sym "+") args)
      describe "Define" $ do
        let call = Call (Sym "define")
        it "Wrong arg number" $ evalDefault (call []) `shouldBe` Right "Define takes exactly two arguments"
        it "Wrong arg type" $ evalDefault (call [Value 3, Value 4]) `shouldBe` Right "Can only define a symbol or call"
        it "Can't evaluate named" $ evalDefault (Call (call [Call (Sym "named_func") [], Call (Sym "undefined") []]) []) `shouldBe` evalDefault (Call (Sym "undefined") [])

      describe "Bad applying" $ do
        it "Value" $ evalDefault (Call (Value 0) []) `shouldBe` Right "Can't apply on number"
        it "Tab" $ evalDefault (Call (Tab []) []) `shouldBe` Right "Can't apply on list"
        it "Boolean" $ evalDefault (Call (Boolean True) []) `shouldBe` Right "Can't apply on boolean"
        it "None" $ evalDefault (Call None []) `shouldBe` Right "Can't apply on an empty function"


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

    describe "rand" $ do
      it "good call" $ case evalAst (Call (Sym "rand") []) (Map.fromList [("seed", Value 7)]) of
        Left (Value v, m) -> (v, m) `shouldBe` (-5295388763150390077, Map.fromList [("seed", Value (-5295388763150390077))])
        v -> fail $ "Expected value but got " ++ show v
      it "with args" $ evalDefault (Call (Sym "rand") [None]) `shouldBe` Right "Rand only takes one argument"
      it "no seed" $ evalAst (Call (Sym "rand") []) Map.empty `shouldBe` Right "Invalid seed, please set it to a value"

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

    describe "Error" $ do
      let ast sym = Call (Sym sym) []
      it "+ no arg" $ evalDefault (ast "+") `shouldBe` Right "Invalid use of add operation"
      it "- no arg" $ evalDefault (ast "-") `shouldBe` Right "Invalid use of sub operation"
      it "* no arg" $ evalDefault (ast "*") `shouldBe` Right "Invalid use of times operation"
      it "div no arg" $ evalDefault (ast "div") `shouldBe` Right "Invalid use of div operation"
      it "mod no arg" $ evalDefault (ast "mod") `shouldBe` Right "Invalid use of mod operation"
      it "** no arg" $ evalDefault (ast "**") `shouldBe` Right "Invalid use of power operation"
      it "& no arg" $ evalDefault (ast "&") `shouldBe` Right "Invalid use of & operation"
      it "| no arg" $ evalDefault (ast "|") `shouldBe` Right "Invalid use of | operation"
      it "^ no arg" $ evalDefault (ast "^") `shouldBe` Right "Invalid use of ^ operation"
      it "~ no arg" $ evalDefault (ast "~") `shouldBe` Right "Invalid use of not operation"
      it "<< no arg" $ evalDefault (ast "<<") `shouldBe` Right "Invalid use of << operation"
      it ">> no arg" $ evalDefault (ast ">>") `shouldBe` Right "Invalid use of >> operation"

      it "eq? no arg" $ evalDefault (ast "eq?") `shouldBe` Right "Invalid use of equal operation"
      it "!= no arg" $ evalDefault (ast "!=") `shouldBe` Right "Invalid use of not equal operation"
      it "< no arg" $ evalDefault (ast "<") `shouldBe` Right "Invalid use of lower than operation"
      it "<= no arg" $ evalDefault (ast "<=") `shouldBe` Right "Invalid use of lower or equal than operation"
      it "> no arg" $ evalDefault (ast ">") `shouldBe` Right "Invalid use of greater than operation"
      it ">= no arg" $ evalDefault (ast ">=") `shouldBe` Right "Invalid use of greater or equal than operation"
      it "and no arg" $ evalDefault (ast "and") `shouldBe` Right "Invalid use of and operation"
      it "or no arg" $ evalDefault (ast "or") `shouldBe` Right "Invalid use of or operation"
      it "not no arg" $ evalDefault (ast "not") `shouldBe` Right "Invalid use of not operation"
      it "if no arg" $ evalDefault (ast "if") `shouldBe` Right "Invalid use of if condition"

      describe "Lists" $ do
        it "car no arg" $ evalDefault (ast "car") `shouldBe` Right "Invalid argument count for car, expected 1 but got 0"
        it "cdr no arg" $ evalDefault (ast "cdr") `shouldBe` Right "Invalid argument count for cdr, expected 1 but got 0"
        it "cons no arg" $ evalDefault (ast "cons") `shouldBe` Right "Invalid argument count for cons, expected 2 but got 0"
        it "isempty no arg" $ evalDefault (ast "isempty") `shouldBe` Right "Invalid argument count for isEmpty, expected 1 but got 0"

        let noList sym = Call (Sym sym) [None]
        it "car not list" $ evalDefault (noList "car") `shouldBe` Right "car function only works on lists"
        it "cdr not list" $ evalDefault (noList  "cdr") `shouldBe` Right "cdr function only works on lists"
        it "isempty not list" $ evalDefault (noList "isempty") `shouldBe` Right "isEmpty only works on lists"

      describe "Wrong types" $ do
        let call sym = Call (Sym sym) [None, None]
        it "+" $ evalDefault (call "+") `shouldBe` Right "Invalid operation"
        it "+" $ evalDefault (call "mod") `shouldBe` Right "Invalid use of mod operation"
        it "+" $ evalDefault (call "div") `shouldBe` Right "Invalid use of div operation"
        it "and" $ evalDefault (call "and") `shouldBe` Right "Invalid operation"
        it "==" $ evalDefault (call "==") `shouldBe` Right "Invalid operation"
        it "if" $ evalDefault (Call (Sym "if") [None, None, None]) `shouldBe` Right "If condition can only evaluate boolean values"
        let emptyCall sym = Call (Sym sym) [Tab []]
        it "car not list" $ evalDefault (emptyCall "car") `shouldBe` Right "Can't apply car on empty list"
        it "cdr not list" $ evalDefault (emptyCall  "cdr") `shouldBe` Right "Can't apply cdr on empty list"

  describe "keywords" $ do
    it "define" $ do
      let expected_result = Map.insert "x" (Value 3) defaultSymbols
      let ast = Call (Sym "define") [Sym "x", Value 3]
      evalAst ast defaultSymbols `shouldBe` Left (None, expected_result)

    it "define named function" $ do
      let expected_result = Map.insert "x" (Lambda undefined) defaultSymbols
      let ast = Call (Sym "define") [Call (Sym "x") [Sym "y"], Value 3]
      evalAst ast defaultSymbols `shouldBe` Left (Lambda undefined, expected_result)

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
    it "empty list" $ printTree (List []) `shouldBe` "An empty list"
    it "boolean" $ printTree (Boolan True) `shouldBe` "A boolean True"

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
      it "parse with spaces" $ parseString "\n 3 \n" `shouldBe` Left (Integer 3)
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

  describe "Error" $ do
    it "unmatched" $ parseString "(hello world" `shouldBe` Right "Unmatched parenthesis"
    it "outside" $ parseString "(hello world) 1 2" `shouldBe` Right "Something is after a parenthesis"
    it "outside list" $ parseString "hello world" `shouldBe` Right "Symbol can't have spaces, did you try to make a list?"
