{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
--}

{-# LANGUAGE ScopedTypeVariables #-}
module LISP.LispMain (lispMain) where

import LISP.Ast ( evalAst, sexprToAST, VarMap )
import Data.Char (toLower)
import LISP.Lib (parseString)
import LISP.Interpreter (parseLineFromFile)
import Control.Exception
import LISP.DefaultSymbol (defaultSymbols)
import System.Exit ( ExitCode(ExitFailure), exitWith, exitSuccess )
import System.Random ( randomIO )
import System.IO
    ( Handle,
      hFlush,
      hPutStrLn,
      openFile,
      stderr,
      stdin,
      stdout,
      IOMode(ReadMode))
import LISP.Types (Ast(..))
import System.Environment
import qualified Data.Map.Lazy as Map

data ExArgs = ExArgs { filename :: String
                    , prompt :: Bool
                    , gladVersion :: Bool
                    , help :: Bool }

processInputFile :: Handle -> IO String
processInputFile file = do
  line <- parseLineFromFile 0 file
  return $ map toLower line

cleanPrintOutput :: Ast -> IO ()
cleanPrintOutput None = return ()
cleanPrintOutput ast = print ast

handleLineFile :: Handle -> String -> VarMap -> IO (Maybe VarMap)
handleLineFile file line m = case parseString line of
  Right err1 -> hPutStrLn stderr ("** ERROR ** : " ++ err1) >> return Nothing
  Left sexpr -> case sexprToAST sexpr of
    Right err2 -> hPutStrLn stderr ("** ERROR ** : " ++ err2) >> return Nothing
    Left ast -> case evalAst ast m of
      Right err3 -> hPutStrLn stderr ("** ERROR ** : " ++ err3) >> return Nothing
      Left (result, newVars) -> cleanPrintOutput result >> loopFile file newVars

loopFile :: Handle -> VarMap -> IO (Maybe VarMap)
loopFile file m = do
  if file == stdin then putStr "Glados> " >> hFlush stdout else putStr ""
  line <- processInputFile file
  case line of
    "quit" -> putStr (if file == stdin then "\n" else "") >> return (Just m)
    _ -> handleLineFile file line m

handleFile :: String -> VarMap -> IO (Maybe VarMap)
handleFile "stdin" m = do
  loopFile stdin m
handleFile fname m = do
  handle (\(e :: IOException) -> hPutStrLn stderr (show e)  >> return Nothing) $ do
    file <- openFile fname ReadMode
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

lispMain :: IO ()
lispMain = do
  args <- getArgs
  let parsedArgs = parseArgs args ExArgs { filename = "", prompt = True, help = False, gladVersion = False }
  handleArgs parsedArgs
  seed <- randomIO :: IO Int
  case args of
    [] -> handleFiles ["stdin"] (Map.insert "seed" (Value seed) defaultSymbols)
    _ -> handleFiles args (Map.insert "seed" (Value seed) defaultSymbols)
