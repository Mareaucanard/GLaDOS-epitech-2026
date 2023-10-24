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

addMain :: [Ast] -> [Ast]
addMain x = case extractFunctions x of
  (a, b) -> a ++ [FunctionDefinition "main" [] b]

compileFile :: Either [Token] String -> Handle -> Bool -> IO ()
compileFile (Right err) _ _ =
  putStrLn "Tokenization failed" >> putStrLn err >> exitWith (ExitFailure 84)
compileFile (Left tokens) outFile b = case tokensToAst
  (applyPreProcessing tokens) of
  Right x -> putStrLn x
  Left ast -> case verifyParsing ast of
    Nothing
      -> f outFile (astListToInstructions $ addMain (simplifyParsing ast))
      >> hFlush outFile
    Just err -> putStrLn err
    where
      f = if b
          then writeHumanReadable
          else writeByteCode

handleSource :: String -> String -> Bool -> IO ()
handleSource fileInputName fileOutputName isHuman = do
  inFile <- streamOpenFile fileInputName ReadMode
  outFile <- streamOpenFile fileOutputName WriteMode
  source <- hGetContents inFile
  compileFile (tokenize source) outFile isHuman

handleVmFile :: String -> String -> IO ()
handleVmFile filename output = do
  byte_code <- BS.readFile filename
  outFile <- streamOpenFile output WriteMode
  case readByteCode byte_code of
    Right err  -> putStrLn err
    Left insts -> writeHumanReadable outFile insts >> hFlush outFile

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
  putStrLn "\t--lisp: uses a lisp interpreter"
  putStrLn "input_file:"
  putStrLn "\tyou can pass 'stdin' as the input file"
  putStrLn "output_file:"
  putStrLn "\tyou can pass 'stdout' as the output file"

doVersion :: IO ()
doVersion = do
  case $(lookupCompileEnvExp "GLADOS_VERSION") of
    Just x -> putStrLn $ "Version " ++ x

main :: IO ()
main = do
  args_in <- getArgs
  case parseArguments args_in of
    Right err -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
    Left args -> do
      let inFile = fromJust $ inputFile args
      let outFile = fromJust $ outputFile args
      case args of
        (Args { help = True })       -> doHelp
        (Args { version = True })    -> doVersion
        (Args { mode = Just Comp })  -> handleSource inFile outFile False
        (Args { mode = Just Exec })  -> handleVmFile inFile outFile
        (Args { mode = Just Human }) -> handleSource inFile outFile True
        (Args { mode = Just Lisp })  -> lispMain inFile outFile
        (Args { mode = Nothing })    -> undefined
