module Main (main) where

import Ast
import Data.Char (toLower)
import Lib (parseString)
import Interpreter (parseLine, parseLineFromFile)
import DefaultSymbol (defaultSymbols)
import System.Exit
import System.Random
import System.IO
import Types (Ast(..))
import System.Environment
import qualified Data.Map.Lazy as Map

data ExArgs = ExArgs { filename :: String
                    , prompt :: Bool
                    , gladVersion :: Bool
                    , help :: Bool }

processInput :: IO String
processInput = do
  line <- parseLine 0
  return $ map toLower line

processInputFile :: Handle -> IO String
processInputFile file = do
  line <- parseLineFromFile 0 file
  return $ map toLower line

cleanPrintOutput :: Ast -> IO ()
cleanPrintOutput None = return ()
cleanPrintOutput ast = print ast

handleLine :: String -> VarMap -> IO ()
handleLine line m = case parseString line of
  Right err1 -> hPutStrLn stderr ("** ERROR ** : " ++ err1)
  Left sexpr -> case sexprToAST sexpr of
    Right err2 -> hPutStrLn stderr ("** ERROR ** : " ++ err2)
    Left ast -> case evalAst ast m of
      Right err3 -> hPutStrLn stderr ("** ERROR ** : " ++ err3)
      Left (result, newVars) -> cleanPrintOutput result >> loop newVars

handleLineFile :: Handle -> String -> VarMap -> IO (Maybe VarMap)
handleLineFile file line m = case parseString line of
  Right err1 -> hPutStrLn stderr ("** ERROR ** : " ++ err1) >> return Nothing
  Left sexpr -> case sexprToAST sexpr of
    Right err2 -> hPutStrLn stderr ("** ERROR ** : " ++ err2) >> return Nothing
    Left ast -> case evalAst ast m of
      Right err3 -> hPutStrLn stderr ("** ERROR ** : " ++ err3) >> return Nothing
      Left (result, newVars) -> cleanPrintOutput result >> loopFile file newVars

-- This is our main loop, it handles when to exit
loop :: VarMap -> IO ()
loop m = do
  putStr "Glados> "
  hFlush stdout
  line <- processInput
  case line of
    "quit" -> return ()
    _ -> handleLine line m

loopFile :: Handle -> VarMap -> IO (Maybe VarMap)
loopFile file m = do
  line <- processInputFile file
  case line of
    "quit" -> return (Just m)
    _ -> handleLineFile file line m

handleFile :: String -> VarMap -> IO (Maybe VarMap)
handleFile toOpen m = do
  file <- openFile toOpen ReadMode
  loopFile file m

handleFiles :: [String] -> VarMap -> IO ()
handleFiles [] _ = return ()
handleFiles (x:xs) m = handleFile x m >>= (\rVal -> case rVal of
  (Just newM) -> handleFiles xs newM
  Nothing -> exitWith (ExitFailure 84))

parseArgs :: [String]
  -> ExArgs
  -> ExArgs
parseArgs [] parsed = parsed
parseArgs (x:xs) parsed = case x of
  "-h" -> parseArgs xs parsed { help = True }
  "--help" -> parseArgs xs parsed { help = True }
  "-p" -> parseArgs xs parsed { prompt = False }
  "--no-prompt" -> parseArgs xs parsed { prompt = False }
  "-v" -> parseArgs xs parsed { gladVersion = True }
  "--version" -> parseArgs xs parsed { gladVersion = True }
  _ -> parseArgs xs parsed { filename = x }

handleArgs :: ExArgs -> IO ()
handleArgs ExArgs { help = True } = putStr "GLaDOS:\n\t-h, --help\tDisplay this help" >>
  putStr "\n\tfile\t\tFile to interpret\n" >>
  exitSuccess
handleArgs ExArgs { gladVersion = True } = putStr "GLaDOS version PLACEHOLDER" >> --add version as env var when implemented in comp
  exitSuccess
handleArgs ExArgs { filename = "" } = hPutStrLn stderr "** ERROR ** : No file given" >>
  exitWith (ExitFailure 84)
handleArgs _ = return ()

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = parseArgs args ExArgs { filename = "", prompt = True, help = False, gladVersion = False }
  handleArgs parsedArgs
  seed <- randomIO :: IO Int
  case args of
    [] -> loop (Map.insert "seed" (Value seed) defaultSymbols)
    _ -> handleFiles args (Map.insert "seed" (Value seed) defaultSymbols)
