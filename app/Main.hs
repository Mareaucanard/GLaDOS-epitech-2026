{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Ast ( evalAst, sexprToAST, VarMap )
import Data.Char (toLower)
import Lib (parseString)
import Interpreter (parseLineFromFile)
import Control.Exception
import DefaultSymbol (defaultSymbols)
import System.Exit ( ExitCode(ExitFailure), exitWith )
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
import Types (Ast(..))
import System.Environment ( getArgs )
import qualified Data.Map.Lazy as Map

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

argsHandler :: [String] -> IO ()
argsHandler [] = putStr "Welcome to GLaDOS, the lisp interpreter\n"
argsHandler _ = return ()

handleFile :: String -> VarMap -> IO (Maybe VarMap)
handleFile "stdin" m = do
  loopFile stdin m
handleFile filename m = do
  handle (\(e :: IOException) -> hPutStrLn stderr (show e)  >> return Nothing) $ do
    file <- openFile filename ReadMode
    loopFile file m

handleFiles :: [String] -> VarMap -> IO ()
handleFiles [] _ = return ()
handleFiles (x:xs) m = handleFile x m >>= (\rVal -> case rVal of
  (Just newM) -> handleFiles xs newM
  Nothing -> exitWith (ExitFailure 84))

main :: IO ()
main = do
  args <- getArgs
  argsHandler args
  seed <- randomIO :: IO Int
  case args of
    [] -> handleFiles ["stdin"] (Map.insert "seed" (Value seed) defaultSymbols)
    _ -> handleFiles args (Map.insert "seed" (Value seed) defaultSymbols)
