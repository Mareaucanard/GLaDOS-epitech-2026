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
                                 , hGetContents, hFlush, hClose)
import           Instructions.ReadByteCode (readByteCode)
import           ParseArguments (parseArguments, Args(..), Mode(..))
import           System.Exit (exitWith, ExitCode(ExitFailure))
import           Data.Maybe (fromJust)
import           LISP.LispMain (lispMain)
import           LookupBuildEnv
import           Vm.Translator (translate)
import           Vm.Instructions (exec)
import           Vm.Builtin.BuiltinMap (builtInMap)
import qualified Data.Map.Lazy as Map
import           Vm.VmTypes

exitError :: String -> IO ()
exitError s = hPutStrLn stderr s >> exitWith (ExitFailure 84)

addMain :: [Ast] -> [Ast]
addMain x = case extractFunctions x of
  (a, b) -> a ++ [FunctionDefinition "main" [] b]

compileFile :: Either [Token] String
            -> Handle
            -> (Handle -> [Instruction] -> IO ())
            -> IO ()
compileFile (Right _) _ _ = exitError "Tokenization failed"
compileFile (Left tokens) outFile f = case tokensToAst
  (applyPreProcessing tokens) of
  Right x  -> exitError x
  Left ast -> case verifyParsing ast of
    Nothing
      -> f outFile (astListToInstructions $ addMain (simplifyParsing ast))
      >> hFlush outFile
      >> hClose outFile
    Just err -> exitError err

handleSource :: String -> String -> (Handle -> [Instruction] -> IO ()) -> IO ()
handleSource fileInputName fileOutputName f = do
  inFile <- streamOpenFile fileInputName ReadMode
  outFile <- streamOpenFile fileOutputName WriteMode
  source <- hGetContents inFile
  compileFile (tokenize source) outFile f

handleVmFile :: [String] -> String -> String -> Bool -> IO ()
handleVmFile args filename output decode = do
  byte_code <- BS.readFile filename
  outFile <- streamOpenFile output WriteMode
  case readByteCode byte_code of
    Right err -> exitError err
    Left inst -> if decode
                 then writeHumanReadable outFile inst
                 else applyRun args outFile inst
  hFlush outFile
  hClose outFile

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

applyRun :: [String] -> Handle -> [Instruction] -> IO ()
applyRun args _ insts = exec new_insts [] fused_map [] >> return ()
  where
    (new_insts, m) = translate insts

    fused_map = Map.fromList
      ([("vargs", Val (Tab (map f args)))]
       ++ Map.toList builtInMap
       ++ Map.toList m)

    f s = V (Str s)

main :: IO ()
main = do
  args_in <- getArgs
  case parseArguments args_in of
    Right err -> exitError err
    Left args -> do
      let inFile = fromJust $ inputFile args
      let outFile = fromJust $ outputFile args
      let pArgs = (progArgs args)
      case args of
        (Args { help = True })        -> doHelp
        (Args { version = True })     -> doVersion
        (Args { mode = Just Comp })
          -> handleSource inFile outFile writeByteCode
        (Args { mode = Just Human })
          -> handleSource inFile outFile writeHumanReadable
        (Args { mode = Just Exec })
          -> handleVmFile pArgs inFile outFile False
        (Args { mode = Just Decode }) -> handleVmFile pArgs inFile outFile True
        (Args { mode = Just Lisp })   -> lispMain inFile outFile
        (Args { mode = Just Run })
          -> handleSource inFile outFile (applyRun pArgs)
        (Args { mode = Nothing })     -> undefined
