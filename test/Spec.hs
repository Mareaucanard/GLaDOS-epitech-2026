import Test.Hspec

import qualified Data.Map.Lazy as Map
import Ast (sexprToAST, evalAst, mapEvalCalls)
import Lib (printTree, parseString, myMaybeMap)
import Types (Ast (..), SExpr (..), VarMap)

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
      let sexpr = Symbol "invalid"
      sexprToAST sexpr `shouldBe` Right "Invalid input: Symbol 'invalid'"

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
      let varMap = Map.fromList []
      evalAst ast varMap `shouldBe` Left (Value 8, varMap)

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
    it "parse umpty string" $ do
      let str = []
      case parseString str of
        Left _ -> fail "Expected Right, but got Left"
        Right expr -> expr `shouldBe` "Can't evaluate empty string"



