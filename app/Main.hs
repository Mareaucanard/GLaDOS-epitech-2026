module Main (main) where

import           System.Environment (getArgs)
import           Parsing.TokenParser
import           Types
import           Parsing.Tokenize (tokenize)
import Parsing.VerifyParsing (simplifyParsing, verifyParsing, extractFunctions)
import Instructions.HumanReadable (writeHumanReadable)
import Instructions.AstToInstructions (astListToInstructions)
import Parsing.PreProcessing (applyPreProcessing)
import Instructions.ByteCode (writeByteCode)
import Data.ByteString as BS (readFile)
import System.IO
import Instructions.ReadByteCode (readByteCode)
import LISP.LispMain (lispMain)


addMain :: [Ast] -> [Ast]
addMain x = case extractFunctions x of
  (a, b) -> a ++ [FunctionDefinition "main" [] b]

printParsed :: Either [Token] String -> Bool -> IO ()
printParsed (Right err) _ = putStrLn "Tokenization failed" >> putStrLn err
printParsed (Left tokens) b = print tokens >> case tokensToAst (applyPreProcessing tokens) of
  Right x  -> putStrLn x
  Left ast -> case verifyParsing ast of
    Nothing ->  f stdout (astListToInstructions $ addMain (ast))
    Just err -> putStrLn err
    where f = if b then writeByteCode else writeHumanReadable


handleVmFile :: String -> IO ()
handleVmFile filename = do
  byte_code <- BS.readFile filename
  case readByteCode byte_code of
    Right err -> putStrLn err
    Left insts -> writeHumanReadable stdout insts

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--encode", str] -> printParsed (tokenize str) True
    ["--human", str] -> printParsed (tokenize str) False
    ["--decode", filename] -> handleVmFile filename
    ("--lisp":xs) -> lispMain xs
    _ -> putStrLn "Please use --encode or --decode"
