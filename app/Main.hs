{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           System.Environment (getArgs, getProgName)
import           Parsing.TokenParser
import           Types
import           Parsing.Tokenize (tokenize)
import           Parsing.VerifyParsing (verifyParsing, extractFunctions
                                      , simplifyParsing)
import           Instructions.HumanReadable (writeHumanReadable)
import           Instructions.AstToInstructions (astListToInstructions)
import           Parsing.PreProcessing (applyPreProcessing)
import           Instructions.ByteCode (writeByteCode)
import qualified Data.ByteString as BS (readFile)
import           System.IO as SIO (Handle, stderr, stdout, hPutStrLn, openFile
                                 , stdin, IOMode(WriteMode, ReadMode)
                                 , hGetContents, hFlush)
import           Instructions.ReadByteCode (readByteCode)
import           ParseArguments (parseArguments, Args(..), Mode(..))
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           Data.Maybe (fromJust)
import           LISP.LispMain (lispMain)
import           LookupBuildEnv
import           Translator (translate)
import           Vm (exec, builtInMap)
import qualified Data.Map.Lazy as Map

addMain :: [Ast] -> [Ast]
addMain x = case extractFunctions x of
  (a, b) -> a ++ [FunctionDefinition "main" [] b]

compileFile :: Either [Token] String
            -> Handle
            -> (Handle -> [Instruction] -> IO ())
            -> IO ()
compileFile (Right err) _ _ = putStrLn "Tokenization failed" >> putStrLn err
compileFile (Left tokens) outFile f = case tokensToAst (applyPreProcessing tokens) of
    Right x  -> putStrLn x
    Left ast -> case verifyParsing ast of
      Nothing
        -> f outFile (astListToInstructions $ addMain (simplifyParsing ast))
        >> hFlush outFile
      Just err -> putStrLn err

handleSource :: String -> String -> (Handle -> [Instruction] -> IO ()) -> IO ()
handleSource fileInputName fileOutputName f = do
  inFile <- streamOpenFile fileInputName ReadMode
  outFile <- streamOpenFile fileOutputName WriteMode
  source <- hGetContents inFile
  compileFile (tokenize source) outFile f

handleVmFile :: String -> String -> Bool -> IO ()
handleVmFile filename output decode = do
  byte_code <- BS.readFile filename
  outFile <- streamOpenFile output WriteMode
  case readByteCode byte_code of
    Right err -> putStrLn err
    Left inst -> if decode
                 then writeHumanReadable outFile inst
                 else print (translate inst)
  hFlush outFile

streamOpenFile :: String -> IOMode -> IO Handle
streamOpenFile "stdin" _ = return stdin
streamOpenFile "stdout" _ = return stdout
streamOpenFile f m = openFile f m

doHelp :: IO ()
doHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <MODE> input_file [-o output_file]"
  putStrLn "Modes:"
  putStrLn "\t--compile: compiles the file to byte code"
  putStrLn "\t--exec: executes a bytecode program"
  putStrLn "\t--human: compiles and prints the list of instructions"
  putStrLn "\t--decode: reads bytecode and prints the list of instructions"
  putStrLn "\t--lisp: uses a lisp interpreter"
  putStrLn "input_file:"
  putStrLn "\tyou can pass 'stdin' as the input file"
  putStrLn "output_file:"
  putStrLn "\tyou can pass 'stdout' as the output file"

doVersion :: IO ()
doVersion = do
  case $(lookupCompileEnvExp "GLADOS_VERSION") of
    Just x -> putStrLn $ "Version " ++ x

applyRun :: Handle -> [Instruction] -> IO ()
applyRun _ insts = exec new_insts [] fused_map [] >> return ()
  where
    (new_insts, m) = translate insts

    fused_map = Map.fromList (Map.toList builtInMap ++ (Map.toList m))

main :: IO ()
main = do
  args_in <- getArgs
  case parseArguments args_in of
    Right err -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
    Left args -> do
      let inFile = fromJust $ inputFile args
      let outFile = fromJust $ outputFile args
      case args of
        (Args { help = True })        -> doHelp
        (Args { version = True })     -> doVersion
        (Args { mode = Just Comp })
          -> handleSource inFile outFile writeByteCode
        (Args { mode = Just Human })
          -> handleSource inFile outFile writeHumanReadable
        (Args { mode = Just Exec })   -> handleVmFile inFile outFile False
        (Args { mode = Just Decode }) -> handleVmFile inFile outFile True
        (Args { mode = Just Lisp })   -> lispMain inFile outFile
        (Args { mode = Just Run })    -> handleSource inFile outFile applyRun
        (Args { mode = Nothing })     -> undefined
