module Encoding (testEncoding) where

import           Test.Hspec
import           Types (Value(..), Instruction(..))
import           Instructions.ReadByteCode (readByteCode)
import qualified Types as T
import           Instructions.ByteCode (byteCodeString)

parseEncode :: String -> [Instruction] -> SpecWith ()
parseEncode s l = it s $ (readByteCode (byteCodeString l)) `shouldBe` (Left l)

testEncoding :: IO ()
testEncoding = hspec
  $ do
    describe "singular command"
      $ do
        describe "push"
          $ do
            parseEncode "int positive" [Push (Integer 1)]
            parseEncode "int negative" [Push (Integer (-1))]
            parseEncode "int zero" [Push (Integer 0)]
            parseEncode "float positive" [Push (Float 3.14)]
            parseEncode "float negative" [Push (Float (-3.14))]
            parseEncode "float zeo" [Push (Float 0.0)]
            parseEncode "char" [Push (Char 'c')]
            parseEncode "str" [Push (Str "Hello")]
            parseEncode "True" [Push (Boolean True)]
            parseEncode "False" [Push (Boolean False)]
            parseEncode "Nil" [Push Nil]
        parseEncode "Function" [Function "double" ["x"]]
        parseEncode "Push symbol" [PushSymbol "x"]
        parseEncode "Jump if false" [JIF 14]
        parseEncode "Jump" [Jump (-20)]
        parseEncode "Call" [Call]
        parseEncode "Set" [Set]
        parseEncode "ADD" [ADD]
        parseEncode "SUB" [SUB]
        parseEncode "MUL" [MUL]
        parseEncode "DIV" [DIV]
        parseEncode "MOD" [MOD]
        parseEncode "AND" [AND]
        parseEncode "OR" [OR]
        parseEncode "NOT" [NOT]
        parseEncode "EQ" [T.EQ]
        parseEncode "NEQ" [NEQ]
        parseEncode "LT" [T.LT]
        parseEncode "LET" [LET]
        parseEncode "GT" [T.GT]
        parseEncode "GET" [GET]
        parseEncode "NEGATIVE" [NEGATIVE]
        parseEncode "RET" [RET]
        parseEncode "List" [LIST 5]
        parseEncode "Index" [INDEX "x"]
