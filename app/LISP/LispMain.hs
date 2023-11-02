{--
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
--}
{-# LANGUAGE ScopedTypeVariables #-}

module LISP.LispMain (lispMain) where

import           LISP.Ast (evalAst, sexprToAST, VarMap)
import           Data.Char (toLower)
import           LISP.Lib (parseString)
import           LISP.Interpreter (parseLineFromFile)
import           LISP.DefaultSymbol (defaultSymbols)
import           System.Exit (ExitCode(ExitFailure), exitWith)
import           System.Random (randomIO)
import           System.IO (Handle, hFlush, hPutStrLn, openFile, stderr, stdin
                          , stdout, IOMode(ReadMode, WriteMode)
                          , hPutStr)
import           LISP.Types (Ast(..))
import qualified Data.Map.Lazy as Map

processInputFile :: Handle -> IO String
processInputFile file = do
  line <- parseLineFromFile 0 file
  return $ map toLower line

cleanPrintOutput :: Ast -> IO ()
cleanPrintOutput None = return ()
cleanPrintOutput ast = print ast


exitError :: IO a
exitError = exitWith (ExitFailure 84)

handleLineFile :: Handle -> Handle -> String -> VarMap -> IO ()
handleLineFile inFile outFile line m = case parseString line of
  Right err1 -> hPutStrLn stderr ("** ERROR ** : " ++ err1) >> exitError
  Left sexpr -> case sexprToAST sexpr of
    Right err2 -> hPutStrLn stderr ("** ERROR ** : " ++ err2) >> exitError
    Left ast   -> case evalAst ast m of
      Right err3 -> hPutStrLn stderr ("** ERROR ** : " ++ err3) >> exitError
      Left (result, newVars)
        -> cleanPrintOutput result >> loopFile inFile outFile newVars

loopFile :: Handle -> Handle -> VarMap -> IO ()
loopFile inFile outFile m = do
  if inFile == stdin
    then hPutStr outFile "Glados> " >> hFlush outFile
    else hPutStr outFile ""
  line <- processInputFile inFile
  case line of
    "quit" -> hPutStr
      outFile
      (if inFile == stdin
       then "\n"
       else "")
    _      -> handleLineFile inFile outFile line m

streamOpenFile :: String -> IOMode -> IO Handle
streamOpenFile "stdin" _ = return stdin
streamOpenFile "stdout" _ = return stdout
streamOpenFile f m = openFile f m

lispMain :: String -> String -> IO ()
lispMain inputFile outputFile = do
  inFile <- streamOpenFile inputFile ReadMode
  outFile <- streamOpenFile outputFile WriteMode
  seed <- randomIO :: IO Int
  loopFile inFile outFile (Map.insert "seed" (Value seed) defaultSymbols)
  hFlush outFile
