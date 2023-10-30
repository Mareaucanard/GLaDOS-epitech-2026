module Main (main) where

import           System.Environment (getArgs)
import           Parsing.TokenParser
import           Types
import           Parsing.Tokenize (tokenize)
import           Parsing.VerifyParsing (simplifyParsing, verifyParsing
                                      , extractFunctions)
import           Instructions.HumanReadable (writeHumanReadable)
import           Instructions.AstToInstructions (astListToInstructions)
import           Parsing.PreProcessing (applyPreProcessing)
import           Instructions.ByteCode (writeByteCode)
import           Data.ByteString as BS (readFile)
import           System.IO
import           Instructions.ReadByteCode (readByteCode)
import           LISP.LispMain (lispMain)
import           Translator (translate)
import           Vm (exec, builtInMap)
import qualified Data.Map.Lazy as Map

addMain :: [Ast] -> [Ast]
addMain x = case extractFunctions x of
  (a, b) -> a ++ [FunctionDefinition "main" [] b]

printParsed :: Either [Token] String -> ([Instruction] -> IO a) -> IO ()
printParsed (Right err) f = putStrLn "Tokenization failed" >> putStrLn err
printParsed (Left tokens) f = case tokensToAst (applyPreProcessing tokens) of
  Right x  -> putStrLn x
  Left ast -> case verifyParsing ast of
    Nothing  -> f (astListToInstructions (addMain ast)) >> return ()
    Just err -> putStrLn err

handleVmFile :: String -> IO ()
handleVmFile filename = do
  byte_code <- BS.readFile filename
  case readByteCode byte_code of
    Right err  -> putStrLn err
    Left insts -> undefined

f filename = do
  file <- openFile filename ReadMode
  str <- hGetContents file
  return str

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--encode", str] -> f str
      >>= \k -> printParsed (tokenize k) (writeHumanReadable stdout)
    ["--human", str] -> f str
      >>= \k -> printParsed (tokenize k) (\x -> v (translate x))
      where
        v (a, b, c, d) = print a
    ["--decode", filename] -> handleVmFile filename
    ["--run", filename] -> f filename >>= \str -> printParsed (tokenize str) j
      where
        j i = exec inst [] tvmap []
          where
            (inst, _, vmap, _) = translate i

            tvmap = Map.fromList ((Map.toList vmap ++ Map.toList builtInMap))
    ("--lisp":xs) -> lispMain xs
    _ -> putStrLn "Please use --encode or --decode"
